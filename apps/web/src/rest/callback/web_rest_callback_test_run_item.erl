-module(web_rest_callback_test_run_item).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include_lib("db/include/protocol_common.hrl").
-include_lib("db/include/protocol_db.hrl").
-include("limits.hrl").
-include("session.hrl").
-include("protocol_data.hrl").
-include("protocol_test_run.hrl").
-include("protocol_notification.hrl").

%% Exported functions

-export([
    get_test_run_items/1,
    get_test_run_item/1,
    create_test_run_items/3,
    update_test_run_item/3,
    start_test_run_item/3,
    pass_test_run_item/3,
    fail_test_run_item/3,
    block_test_run_item/3,
    reopen_test_run_item/3,
    delete_test_run_item/2
]).

-record(test_item, {
    case_id :: non_neg_integer(),
    setup_ids :: [non_neg_integer()],
    specs :: [protocol_db:specialized_setup()],
    params :: maps:map()
}).

-record(walk_state, {
    base_case_id :: non_neg_integer(),
    can_spread = false :: boolean,
    params :: maps:map(),
    setup :: [protocol_db:setup_step()],
    items :: [#test_item{}]
}).

-define(ANY, <<"ANY">>).

%% API

-spec get_test_run_items(RunId :: non_neg_integer()) ->
    protocol_data:collection(protocol_db:test_run_item()).

get_test_run_items(RunId) ->
    case db_if_test_run_items:get_all(RunId) of
        {ok, Items} -> #collection{items = Items};
        {error, Reason} -> web_rest_test_run_items_run:get_test_run_items_500(#internal_server_error{error = Reason})
    end.

-spec get_test_run_item(Id :: non_neg_integer()) ->
    protocol_db:test_run_item().

get_test_run_item(Id) ->
    case db_if_test_run_items:get_one(Id) of
        {ok, Item} -> Item;
        {error, ?err_not_exists} -> web_rest_test_run_item:get_test_run_item_404(#not_found_error{});
        {error, Reason} -> web_rest_test_run_item:get_test_run_item_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec create_test_run_items(Request, RunId, Req) -> Result when
    Request :: protocol_test_run:create_test_run_item_request(),
    RunId :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {protocol_data:collection(protocol_db:test_run_item()), cowboy_req:req()}.

create_test_run_items(#create_test_run_item_request{} = Request, RunId, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    #create_test_run_item_request{case_id = CaseId, order_num = OrderNum} = Request,
    {ok, ProjectId} = db_if_test_cases:project_id(CaseId),
    {ok, ProjectParams} = db_if_parameters:get_for_project(ProjectId),
    SpawnedItems = spawn_items(CaseId),
    ItemCount = length(SpawnedItems),
    SpawnedItemsFiltered = lists:filtermap(fun(#test_item{specs = Specs}) ->
        ItemParamMap = lists:foldl(fun(#specialized_setup{param = P, value = V}, Acc) -> Acc#{P => V} end, #{}, Specs),
        is_viable_item(ItemParamMap, ProjectParams)
    end, SpawnedItems),
    TestRunItems = [try
        Params = #test_run_item_params{setup_steps = SetupIds, specs = Specs},
        {ok, ItemId} = db_if_test_run_items:create(RunId, RelCaseId, UserId, OrderNum, Params),
        {ok, Item} = db_if_test_run_items:get_one(ItemId),
        Item
        catch _T:_W:_S -> undefined
    end || #test_item{case_id = RelCaseId, setup_ids = SetupIds, specs = Specs} <- lists:usort(SpawnedItemsFiltered)],
    TestRunItemsFiltered = [TRI || TRI <- TestRunItems, TRI =/= undefined],
    [web_ws:broadcast(#test_created{actor_id = UserId, actor_name = Username, data = TRI}) || TRI <- TestRunItemsFiltered],
    ?doif(ItemCount > 0, notify_test_run_updated(UserId, Username, RunId)),
    {#collection{items = TestRunItemsFiltered}, Req}.

-spec update_test_run_item(Request :: protocol_test_run:update_test_run_item_request(), Id :: non_neg_integer(), Req :: cowboy_req:req()) ->
    {protocol_db:test_run_item(), cowboy_req:req()}.

update_test_run_item(#update_test_run_item_request{} = Request, Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    Patch = protocol_test_run:update_test_run_item_request_to_json(Request),
    case maps:size(Patch) of
        0 -> web_rest_test_run_item:update_test_run_item_400(#bad_request_error{error = nothing_to_update});
        _ ->
            case db_if_test_run_items:update(Id, Patch) of
                ok ->
                    case db_if_test_run_items:get_one(Id) of
                        {ok, Item} ->
                            web_ws:broadcast(#test_updated{actor_id = UserId, actor_name = Username, data = Item}),
                            {Item, Req};
                        {error, Reason} ->
                            web_rest_test_run_item:update_test_run_item_500(#internal_server_error{error = util_binary:to_binary(Reason)})
                    end;
                {error, ?err_not_exists} ->
                    web_rest_test_run_item:update_test_run_item_404(#not_found_error{});
                {error, Reason} ->
                    case is_bad_request(Reason) of
                        true -> web_rest_test_run_item:update_test_run_item_400(#bad_request_error{error = Reason});
                        false -> web_rest_test_run_item:update_test_run_item_500(#internal_server_error{error = util_binary:to_binary(Reason)})
                    end
            end
    end.

-spec start_test_run_item(Request :: protocol_common:empty(), Id :: non_neg_integer(), Req :: cowboy_req:req()) ->
    {protocol_db:test_run_item(), cowboy_req:req()}.

start_test_run_item(#empty{}, Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    case db_if_test_run_items:get_one(Id) of
        {ok, #test_run_item{status = pending, run_id = RunId}} ->
            Patch = #{
                <<"status">> => protocol_db:test_status_to_json(in_progress),
                <<"assignee_id">> => UserId,
                <<"started_at">> => util_time:utc_datetime()
            },
            case db_if_test_run_items:update(Id, Patch) of
                ok ->
                    case db_if_test_run_items:get_one(Id) of
                        {ok, Item} ->
                            web_ws:broadcast(#test_started{actor_id = UserId, actor_name = Username, data = Item}),
                            notify_test_run_updated(UserId, Username, RunId),
                            {Item, Req};
                        {error, Reason} ->
                            web_rest_test_run_item_start:start_test_run_item_500(#internal_server_error{error = util_binary:to_binary(Reason)})
                    end;
                {error, ?err_not_exists} ->
                    web_rest_test_run_item_start:start_test_run_item_404(#not_found_error{});
                {error, Reason} ->
                    case is_bad_request(Reason) of
                        true -> web_rest_test_run_item_start:start_test_run_item_400(#bad_request_error{error = Reason});
                        false -> web_rest_test_run_item_start:start_test_run_item_500(#internal_server_error{error = util_binary:to_binary(Reason)})
                    end
            end;
        {ok, #test_run_item{status = in_progress}} -> web_rest_test_run_item_start:start_test_run_item_400(#bad_request_error{error = already_started});
        {ok, _} -> web_rest_test_run_item_start:start_test_run_item_400(#bad_request_error{error = already_finished});
        {error, Reason} -> web_rest_test_run_item_start:start_test_run_item_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec pass_test_run_item(Request :: protocol_common:finish_test_run_item_request(), Id :: non_neg_integer(), Req :: cowboy_req:req()) ->
    {protocol_db:test_run_item(), cowboy_req:req()}.

pass_test_run_item(#finish_test_run_item_request{summary = Summary}, Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    case db_if_test_run_items:get_one(Id) of
        {ok, #test_run_item{status = Status, run_id = RunId}} when Status =:= pending; Status =:= in_progress ->
            Patch = #{
                <<"status">> => protocol_db:test_status_to_json(passed),
                <<"summary">> => ?yesno(is_binary(Summary), Summary, ?null),
                <<"assignee_id">> => UserId,
                <<"started_at">> => util_time:utc_datetime(),
                <<"finished_at">> => util_time:utc_datetime()
            },
            case db_if_test_run_items:update(Id, Patch) of
                ok ->
                    case db_if_test_run_items:get_one(Id) of
                        {ok, Item} ->
                            web_ws:broadcast(#test_passed{actor_id = UserId, actor_name = Username, data = Item}),
                            notify_test_run_updated(UserId, Username, RunId),
                            {Item, Req};
                        {error, Reason} ->
                            web_rest_test_run_item_pass:pass_test_run_item_500(#internal_server_error{error = util_binary:to_binary(Reason)})
                    end;
                {error, ?err_not_exists} ->
                    web_rest_test_run_item_pass:pass_test_run_item_404(#not_found_error{});
                {error, Reason} ->
                    case is_bad_request(Reason) of
                        true -> web_rest_test_run_item_pass:pass_test_run_item_400(#bad_request_error{error = Reason});
                        false -> web_rest_test_run_item_pass:pass_test_run_item_500(#internal_server_error{error = util_binary:to_binary(Reason)})
                    end
            end;
        {ok, _} -> web_rest_test_run_item_pass:pass_test_run_item_400(#bad_request_error{error = already_finished});
        {error, Reason} -> web_rest_test_run_item_pass:pass_test_run_item_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec fail_test_run_item(Request :: protocol_common:finish_test_run_item_request(), Id :: non_neg_integer(), Req :: cowboy_req:req()) ->
    {protocol_db:test_run_item(), cowboy_req:req()}.

fail_test_run_item(#finish_test_run_item_request{summary = Summary}, Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    case db_if_test_run_items:get_one(Id) of
        {ok, #test_run_item{status = Status, run_id = RunId}} when Status =:= pending; Status =:= in_progress ->
            Patch = #{
                <<"status">> => protocol_db:test_status_to_json(failed),
                <<"summary">> => ?yesno(is_binary(Summary), Summary, ?null),
                <<"assignee_id">> => UserId,
                <<"started_at">> => util_time:utc_datetime(),
                <<"finished_at">> => util_time:utc_datetime()
            },
            case db_if_test_run_items:update(Id, Patch) of
                ok ->
                    case db_if_test_run_items:get_one(Id) of
                        {ok, Item} ->
                            web_ws:broadcast(#test_failed{actor_id = UserId, actor_name = Username, data = Item}),
                            notify_test_run_updated(UserId, Username, RunId),
                            {Item, Req};
                        {error, Reason} ->
                            web_rest_test_run_item_fail:fail_test_run_item_500(#internal_server_error{error = util_binary:to_binary(Reason)})
                    end;
                {error, ?err_not_exists} ->
                    web_rest_test_run_item_fail:fail_test_run_item_404(#not_found_error{});
                {error, Reason} ->
                    case is_bad_request(Reason) of
                        true -> web_rest_test_run_item_fail:fail_test_run_item_400(#bad_request_error{error = Reason});
                        false -> web_rest_test_run_item_fail:fail_test_run_item_500(#internal_server_error{error = util_binary:to_binary(Reason)})
                    end
            end;
        {ok, _} -> web_rest_test_run_item_fail:fail_test_run_item_400(#bad_request_error{error = already_finished});
        {error, Reason} -> web_rest_test_run_item_fail:fail_test_run_item_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec block_test_run_item(Request :: protocol_common:block_test_run_item_request(), Id :: non_neg_integer(), Req :: cowboy_req:req()) ->
    {protocol_db:test_run_item(), cowboy_req:req()}.

block_test_run_item(Request, Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    #block_test_run_item_request{failed_setup_id = FailedSetupId, setup_parameter_value = SetupParamValue, summary = Summary} = Request,
    case db_if_test_run_items:get_one(Id) of
        {ok, #test_run_item{status = Status, run_id = RunId}} when Status =:= pending; Status =:= in_progress ->
            Patch = #{
                <<"status">> => protocol_db:test_status_to_json(blocked),
                <<"failed_setup_id">> => FailedSetupId,
                <<"summary">> => ?yesno(is_binary(Summary), Summary, ?null),
                <<"assignee_id">> => UserId,
                <<"started_at">> => util_time:utc_datetime(),
                <<"finished_at">> => util_time:utc_datetime()
            },
            case db_if_test_run_items:update(Id, Patch) of
                ok ->
                    % Block all related test run items
                    {ok, RelatedItems} = db_if_test_run_items:get_for_setup(RunId, FailedSetupId, true),

                    % Filter out items with unrelated setup parameter value
                    ActualItems = ?yesno(SetupParamValue =:= undefined, RelatedItems, begin
                        FnCheckSpec = fun
                            (#specialized_setup{setup_id = S, value = V}) when S =:= FailedSetupId, V =:= SetupParamValue -> true;
                            (_) -> false
                        end,
                        [TRI || #test_run_item{params = #test_run_item_params{specs = Specs}} = TRI <- RelatedItems,
                                lists:any(fun(Spec) -> FnCheckSpec(Spec) end, Specs)]
                    end),

                    [begin
                        ok = db_if_test_run_items:update(RelatedId, Patch),
                        {ok, RelatedItem} = db_if_test_run_items:get_one(RelatedId),
                        web_ws:broadcast(#test_blocked{actor_id = UserId, actor_name = Username, data = RelatedItem})
                    end || #test_run_item{id = RelatedId} <- ActualItems],

                    % Return updated item
                    case db_if_test_run_items:get_one(Id) of
                        {ok, Item} ->
                            web_ws:broadcast(#test_blocked{actor_id = UserId, actor_name = Username, data = Item}),
                            notify_test_run_updated(UserId, Username, RunId),
                            {Item, Req};
                        {error, Reason} ->
                            web_rest_test_run_item_block:block_test_run_item_500(#internal_server_error{error = util_binary:to_binary(Reason)})
                    end;
                {error, ?err_not_exists} ->
                    web_rest_test_run_item_block:block_test_run_item_404(#not_found_error{});
                {error, Reason} ->
                    case is_bad_request(Reason) of
                        true -> web_rest_test_run_item_block:block_test_run_item_400(#bad_request_error{error = Reason});
                        false -> web_rest_test_run_item_block:block_test_run_item_500(#internal_server_error{error = util_binary:to_binary(Reason)})
                    end
            end;
        {ok, _} -> web_rest_test_run_item_block:block_test_run_item_400(#bad_request_error{error = already_finished});
        {error, Reason} -> web_rest_test_run_item_block:block_test_run_item_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec reopen_test_run_item(Request :: protocol_common:empty(), Id :: non_neg_integer(), Req :: cowboy_req:req()) ->
    {protocol_db:test_run_item(), cowboy_req:req()}.

reopen_test_run_item(#empty{}, Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    case db_if_test_run_items:get_one(Id) of
        {ok, #test_run_item{status = Status, run_id = RunId}} when Status =:= passed; Status =:= failed; Status =:= blocked ->
            Patch = #{
                <<"status">> => protocol_db:test_status_to_json(in_progress),
                <<"failed_setup_id">> => ?null,
                <<"summary">> => ?null,
                <<"assignee_id">> => UserId,
                <<"started_at">> => util_time:utc_datetime(),
                <<"finished_at">> => ?null
            },
            case db_if_test_run_items:update(Id, Patch) of
                ok ->
                    % Return updated item
                    case db_if_test_run_items:get_one(Id) of
                        {ok, Item} ->
                            web_ws:broadcast(#test_reopened{actor_id = UserId, actor_name = Username, data = Item}),
                            notify_test_run_updated(UserId, Username, RunId),
                            {Item, Req};
                        {error, Reason} ->
                            web_rest_test_run_item_reopen:reopen_test_run_item_500(#internal_server_error{error = util_binary:to_binary(Reason)})
                    end;
                {error, ?err_not_exists} ->
                    web_rest_test_run_item_reopen:reopen_test_run_item_404(#not_found_error{});
                {error, Reason} ->
                    case is_bad_request(Reason) of
                        true -> web_rest_test_run_item_reopen:reopen_test_run_item_400(#bad_request_error{error = Reason});
                        false -> web_rest_test_run_item_reopen:reopen_test_run_item_500(#internal_server_error{error = util_binary:to_binary(Reason)})
                    end
            end;
        {ok, _} -> web_rest_test_run_item_reopen:reopen_test_run_item_400(#bad_request_error{error = already_opened});
        {error, Reason} -> web_rest_test_run_item_reopen:reopen_test_run_item_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec delete_test_run_item(Id :: non_neg_integer(), Req :: cowboy_req:req()) ->
    {protocol_data:generic_response(), cowboy_req:req()}.

delete_test_run_item(Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    case db_if_test_run_items:get_one(Id) of
        {ok, #test_run_item{run_id = RunId} = Item} ->
            case db_if_test_run_items:delete(Id) of
                ok ->
                    web_ws:broadcast(#test_deleted{actor_id = UserId, actor_name = Username, data = Item}),
                    notify_test_run_updated(UserId, Username, RunId),
                    {#generic_response{result = true}, Req};
                {error, ?err_not_exists} -> web_rest_test_run_item:delete_test_run_item_404(#not_found_error{});
                {error, Reason} -> web_rest_test_run_item:delete_test_run_item_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, ?err_not_exists} -> web_rest_test_run_item:delete_test_run_item_404(#not_found_error{});
        {error, Reason} -> web_rest_test_run_item:delete_test_run_item_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

%% Local functions

is_bad_request(assignee_not_exists) -> true;
is_bad_request(reporter_not_exists) -> true;
is_bad_request(setup_not_exists) -> true;
is_bad_request(test_case_not_exists) -> true;
is_bad_request(test_run_not_exists) -> true;
is_bad_request(_) -> false.

notify_test_run_updated(UserId, Username, RunId) ->
    {ok, TestRun} = db_if_test_runs:get_one(RunId),
    #test_run{
        project_id = ProjectId,
        title = RunTitle,
        status = Status,
        started_at = StartedAt,
        total_item_count = NumTotal,
        passed_item_count = NumSuccess,
        blocked_item_count = NumBlocked,
        failed_item_count = NumFailed
    } = TestRun,

    % Notify via Slack
    LooksLikeComplete = StartedAt =/= undefined andalso Status =:= in_progress andalso (NumTotal =:= NumSuccess + NumBlocked + NumFailed),
    ?doif(LooksLikeComplete, begin
        {ok, #project{key = ProjectKey, slack_receivers = SlackReceivers}} = db_if_projects:get_one(ProjectId),
        ?doif(is_binary(SlackReceivers) andalso SlackReceivers =/= <<>>, begin
            ReceiverIds = [util_binary:trim(SR) || SR <- binary:split(SlackReceivers, <<",">>, [global])],
            [slack:test_run_completed(SR, ProjectKey, RunId, RunTitle, NumSuccess, NumBlocked, NumFailed) || SR <- ReceiverIds, SR =/= <<>>]
        end)
    end),

    % Notify via Websocket
    web_ws:broadcast(#test_run_updated{actor_id = UserId, actor_name = Username, data = TestRun}).

%% Test run item generation

parameter(ParamId) ->
    case db_if_parameters:get_one(ParamId) of
        {ok, Param} -> Param;
        _ -> undefined
    end.

setup_ids(CaseId) ->
    {ok, SetupId} = db_if_test_cases:setup_id(CaseId),
    setup_ids(SetupId, [SetupId]).

setup_ids(undefined, Acc) ->
    Acc;
setup_ids(SetupId, Acc) ->
    {ok, ParentId} = db_if_setup_steps:parent_id(SetupId),
    Acc1 = ?yesno(is_integer(ParentId), [ParentId|Acc], Acc),
    setup_ids(ParentId, Acc1).

test_case(CaseId) ->
    {ok, Case} = db_if_test_cases:get_one(CaseId),
    Case.

parent_case_ids(CaseId) ->
    parent_case_ids(CaseId, []).

parent_case_ids(undefined, Acc) ->
    Acc;
parent_case_ids(CaseId, Acc) ->
    {ok, ParentId} = db_if_test_cases:parent_id(CaseId),
    Acc1 = ?yesno(is_integer(ParentId), [ParentId|Acc], Acc),
    parent_case_ids(ParentId, Acc1).

normalize_params(SetupIds, Params) ->
    normalize_params(SetupIds, Params, #{}).

normalize_params([], _ParamsIn, ParamsOut) ->
    ParamsOut;
normalize_params([SetupId | Rest], ParamsIn, ParamsOut) ->
    LinkedParamId = case db_if_setup_steps:linked_parameter_id(SetupId) of
        {ok, Id} -> Id;
        _ -> undefined
    end,
    ParamsOut1 = case maps:get(SetupId, ParamsIn, undefined) of
        undefined ->
            ParamsOut;
        #test_case_specialization{spec_type = value, value = Value} ->
            #parameter{title = ParamKey} = parameter(LinkedParamId),
            ParamsOut#{{SetupId, ParamKey} => [Value]};
        #test_case_specialization{spec_type = source, parameter_id = ParamId} ->
            #parameter{title = ParamKey, values = Values, dependent_id = DependentId} = parameter(ParamId),
            DependentKey = case parameter(DependentId) of
                undefined -> undefined;
                #parameter{title = DK} -> DK
            end,
            Values1 = lists:filtermap(fun
                (#parameter_value{dependent_value = undefined, value = V}) ->
                    {true, V};
                (#parameter_value{dependent_value = DV, value = V}) ->
                    OutKeys = [K || {_SID, PK} = K <- maps:keys(ParamsOut), PK =:= DependentKey],
                    DPV = ?yesno(OutKeys =:= [], [], begin
                        ComplexKey = lists:last(OutKeys),
                        maps:get(ComplexKey, ParamsOut, [])
                    end),
                    ?yesno(lists:member(DV, DPV), {true, V}, false);
                (_) -> false
            end, Values),
            ParamsOut#{{SetupId, ParamKey} => Values1};
        #test_case_specialization{spec_type = any} ->
            #parameter{title = ParamKey} = parameter(LinkedParamId),
            ParamsOut#{{SetupId, ParamKey} => [?ANY]};
        #test_case_specialization{spec_type = random} ->
            #parameter{title = ParamKey, values = Values, dependent_id = DependentId} = parameter(LinkedParamId),
            DependentKey = case parameter(DependentId) of
                undefined -> undefined;
                #parameter{title = DK} -> DK
            end,
            Values1 = lists:filtermap(fun
                (#parameter_value{dependent_value = undefined, value = V}) ->
                    {true, V};
                (#parameter_value{dependent_value = DV, value = V}) ->
                    OutKeys = [K || {_SID, PK} = K <- maps:keys(ParamsOut), PK =:= DependentKey],
                    DPV = ?yesno(OutKeys =:= [], [], begin
                        ComplexKey = lists:last(OutKeys),
                        maps:get(ComplexKey, ParamsOut, [])
                    end),
                    ?yesno(lists:member(DV, DPV), {true, V}, false);
                (_) -> false
            end, Values),
            ParamsOut#{{SetupId, ParamKey} => rand_list:select(Values1)}
    end,
    normalize_params(Rest, ParamsIn, ParamsOut1).

walk_specs([], _Specs, Params) ->
    Params;
walk_specs([SetupId | Rest], Specs, Params) ->
    Params1 = case [Spec || #test_case_specialization{setup_id = Id} = Spec <- Specs, Id =:= SetupId] of
        [Spec|_] -> Params#{SetupId => Spec};
        _ -> Params
    end,
    walk_specs(Rest, Specs, Params1).

walk_cases(BaseCaseId) ->
    CaseIds = parent_case_ids(BaseCaseId) ++ [BaseCaseId],
    Cases = [test_case(CaseId) || CaseId <- CaseIds],
    TestItems = walk_cases(Cases, #walk_state{base_case_id = BaseCaseId, params = #{}, setup = [], items = []}),
    [T#test_item{params = normalize_params(SetupIds, Params)} || #test_item{setup_ids = SetupIds, params = Params} = T <- TestItems].

walk_cases([], #walk_state{items = Items}) ->
    Items;
walk_cases([Case | Rest], #walk_state{base_case_id = BaseCaseId, can_spread = CanSpread, setup = InheritedSetupIds, params = Params, items = Items} = State) ->
    #test_case{id = CaseId, specs = Specs, is_group = IsGroup} = Case,
    SetupIds = setup_ids(CaseId),
    ActualCanSpread = CanSpread orelse CaseId =:= BaseCaseId,
    ActualSetupIds = ?yesno(SetupIds =/= [], SetupIds, InheritedSetupIds),
    ActualSetupIdsFiltered = [SID || SID <- ActualSetupIds, is_integer(SID)],
    ActualParams = walk_specs(ActualSetupIdsFiltered, Specs, Params),
    State1 = State#walk_state{can_spread = ActualCanSpread, setup = ActualSetupIdsFiltered, params = ActualParams},
    case ActualCanSpread of
        true ->
            case IsGroup of
                true ->
                    {ok, Children} = db_if_test_cases:get_children(CaseId),
                    State2 = lists:foldl(fun(ChildCase, #walk_state{items = AccItems} = AccState) ->
                        ChildItems = walk_cases([ChildCase], State1),
                        AccState#walk_state{items = AccItems ++ ChildItems}
                    end, State1, Children),
                    walk_cases([], State2);
                false ->
                    TestItem = #test_item{case_id = CaseId, setup_ids = ActualSetupIdsFiltered, params = maps:with(ActualSetupIdsFiltered, ActualParams)},
                    walk_cases([], State1#walk_state{items = [TestItem | Items]})
            end;
        false ->
            walk_cases(Rest, State1)
    end.

setup_specs(SetupIds, Params) ->
    setup_specs(SetupIds, Params, []).

setup_specs([], _Params, Specs) ->
    lists:reverse(Specs);
setup_specs([SetupId | Rest], Params, Specs) ->
    AcceptableKeys = [K || {SID, _} = K <- maps:keys(Params), SID =:= SetupId],
    case AcceptableKeys of
        [] -> setup_specs(Rest, Params, Specs);
        _ ->
            Key = lists:last(AcceptableKeys),
            Spec = case maps:get(Key, Params, undefined) of
                Value when is_binary(Value) ->
                    {SetupId, ParamKey} = Key,
                    #specialized_setup{setup_id = SetupId, param = ParamKey, value = Value};
                _ -> undefined
            end,
            Specs1 = ?yesno(Spec =/= undefined, [Spec | Specs], Specs),
            setup_specs(Rest, Params, Specs1)
    end.

spawn_items(CaseId) ->
    TestItems = walk_cases(CaseId),
    ItemsToCreate = [begin
        ParamList = [[{K, V} || V <- Vs] || {K, Vs} <- maps:to_list(Params)],
        ParamCount = length(ParamList),
        SafeParamList = lists:sublist(ParamList, min(ParamCount, 10)),
        PermFn = case ParamCount of
            0 -> fun(X) -> X end;
            1 -> fun web_util:naive_permutations/1;
            2 -> fun web_util:naive_permutations/2;
            3 -> fun web_util:naive_permutations/3;
            4 -> fun web_util:naive_permutations/4;
            5 -> fun web_util:naive_permutations/5;
            6 -> fun web_util:naive_permutations/6;
            7 -> fun web_util:naive_permutations/7;
            8 -> fun web_util:naive_permutations/8;
            9 -> fun web_util:naive_permutations/9;
            10 -> fun web_util:naive_permutations/10
        end,
        case SafeParamList of
            [] -> [TI#test_item{params = #{}}];
            _ -> [TI#test_item{params = P} || P <- erlang:apply(PermFn, SafeParamList)]
        end
    end || #test_item{params = Params} = TI <- TestItems],
    FlattenedItems = lists:flatten(ItemsToCreate),
    [TI#test_item{specs = setup_specs(SetupIds, Params)} || #test_item{setup_ids = SetupIds, params = Params} = TI <- FlattenedItems].

is_viable_item(ItemParamMap, ProjectParams) ->
    ParamIdMap = lists:foldl(fun(#parameter{id = Id} = P, Acc) -> Acc#{Id => P} end, #{}, ProjectParams),
    ParamKeyMap = lists:foldl(fun(#parameter{title = T} = P, Acc) -> Acc#{T => P} end, #{}, ProjectParams),
    maps:fold(fun
        (_ParamKey, _Value, false) ->
            false;
        (ParamKey, Value, _) ->
            case maps:get(ParamKey, ParamKeyMap) of
                #parameter{dependent_id = undefined} -> true;
                #parameter{dependent_id = DependentId, values = ValuePairs} ->
                    #parameter{title = DependentKey} = maps:get(DependentId, ParamIdMap),
                    DependentValue = maps:get(DependentKey, ItemParamMap),
                    [V || #parameter_value{dependent_value = DV, value = V} <- ValuePairs, V =:= Value, DependentValue =:= DV] =/= []
            end
    end, true, ItemParamMap).
