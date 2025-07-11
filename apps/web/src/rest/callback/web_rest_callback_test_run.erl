-module(web_rest_callback_test_run).

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
    get_test_runs/6,
    get_test_run/1,
    create_test_run/3,
    start_test_run/3,
    close_test_run/3,
    reopen_test_run/3,
    clone_test_run/3,
    update_test_run/3,
    delete_test_run/2
]).

%% API

-spec get_test_runs(ProjectId, OrderBy, OrderDir, Offset, Limit, Status) -> Result when
    ProjectId :: non_neg_integer(),
    OrderBy :: protocol_test_run:test_run_order_by(),
    OrderDir :: protocol_data:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Status :: boolean() | 'undefined',
    Result :: protocol_data:collection_slice(protocol_db:test_run()).

get_test_runs(ProjectId, OrderBy, OrderDir, Offset, Limit, Status) ->
    Status1 = ?yesno(Status =:= undefined, ?null, protocol_db:test_run_status_to_json(Status)),
    {ok, TestRuns} = db_if_test_runs:get(ProjectId, util_binary:to_binary(OrderBy), util_binary:to_binary(OrderDir), Offset, ?slice_limit(Limit), Status1),
    {ok, Total} = db_if_test_runs:get_count(ProjectId, Status1),
    #collection_slice{items = TestRuns, total = Total}.

-spec get_test_run(Id :: non_neg_integer()) ->
    protocol_db:test_run().

get_test_run(Id) ->
    case db_if_test_runs:get_one(Id) of
        {ok, TestCase} -> TestCase;
        {error, ?err_not_exists} -> web_rest_test_run:get_test_run_404(#not_found_error{});
        {error, Reason} -> web_rest_test_run:get_test_run_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec create_test_run(Request, ProjectId, Req) -> Result when
    Request :: protocol_test_run:create_test_run_request(),
    ProjectId :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {protocol_db:test_run(), Req :: cowboy_req:req()}.

create_test_run(#create_test_run_request{title = Title}, ProjectId, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    case db_if_test_runs:create(ProjectId, Title) of
        {ok, TestRunId} ->
            case db_if_test_runs:get_one(TestRunId) of
                {ok, TestRun} ->
                    web_ws:broadcast(#test_run_created{actor_id = UserId, actor_name = Username, data = TestRun}),
                    {TestRun, Req};
                {error, Reason} ->
                    web_rest_test_runs:create_test_run_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, Reason} ->
            case is_bad_request(Reason) of
                true -> web_rest_test_runs:create_test_run_400(#bad_request_error{error = Reason});
                false -> web_rest_test_runs:create_test_run_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end
    end.

-spec start_test_run(Request :: protocol_common:empty(), Id :: non_neg_integer(), Req :: cowboy_req:req()) ->
    {protocol_db:test_run(), Req :: cowboy_req:req()}.

start_test_run(#empty{}, Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    case db_if_test_runs:status(Id) of
        {ok, draft} ->
            Patch = #{
                <<"status">> => protocol_db:test_run_status_to_json(in_progress),
                <<"started_at">> => util_time:utc_datetime()
            },
            case db_if_test_runs:update(Id, Patch) of
                ok ->
                    case db_if_test_runs:get_one(Id) of
                        {ok, #test_run{project_id = ProjectId, title = TestRunTitle, total_item_count = NumTotal} = TestRun} ->
                            {ok, #project{key = ProjectKey, slack_receivers = SlackReceivers}} = db_if_projects:get_one(ProjectId),
                            ?doif(is_binary(SlackReceivers) andalso SlackReceivers =/= <<>>, begin
                                ReceiverIds = [util_binary:trim(SR) || SR <- binary:split(SlackReceivers, <<",">>, [global])],
                                [slack:test_run_started(SR, ProjectKey, Id, TestRunTitle, NumTotal) || SR <- ReceiverIds, SR =/= <<>>]
                            end),
                            web_ws:broadcast(#test_run_started{actor_id = UserId, actor_name = Username, data = TestRun}),
                            {TestRun, Req};
                        {error, Reason} ->
                            web_rest_test_run_start:start_test_run_500(#internal_server_error{error = util_binary:to_binary(Reason)})
                    end;
                {error, ?err_not_exists} ->
                    web_rest_test_run_start:start_test_run_404(#not_found_error{});
                {error, Reason} ->
                    web_rest_test_run_start:start_test_run_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {ok, in_progress} -> web_rest_test_run_start:start_test_run_400(#bad_request_error{error = already_started});
        {ok, closed} -> web_rest_test_run_start:start_test_run_400(#bad_request_error{error = already_closed});
        {error, Reason} -> web_rest_test_run_start:start_test_run_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec close_test_run(Request :: protocol_common:empty(), Id :: non_neg_integer(), Req :: cowboy_req:req()) ->
    {protocol_db:test_run(), Req :: cowboy_req:req()}.

close_test_run(#empty{}, Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    case db_if_test_runs:status(Id) of
        {ok, Status} when Status =/= closed ->
            Patch = #{<<"status">> => protocol_db:test_run_status_to_json(closed)},
            case db_if_test_runs:update(Id, Patch) of
                ok ->
                    case db_if_test_runs:get_one(Id) of
                        {ok, TestRun} ->
                            web_ws:broadcast(#test_run_closed{actor_id = UserId, actor_name = Username, data = TestRun}),
                            {TestRun, Req};
                        {error, Reason} ->
                            web_rest_test_run_close:close_test_run_500(#internal_server_error{error = util_binary:to_binary(Reason)})
                    end;
                {error, ?err_not_exists} ->
                    web_rest_test_run_close:close_test_run_404(#not_found_error{});
                {error, Reason} ->
                    web_rest_test_run_close:close_test_run_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {ok, closed} -> web_rest_test_run_close:close_test_run_400(#bad_request_error{error = already_closed});
        {error, Reason} -> web_rest_test_run_close:close_test_run_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec reopen_test_run(Request :: protocol_common:empty(), Id :: non_neg_integer(), Req :: cowboy_req:req()) ->
    {protocol_db:test_run(), Req :: cowboy_req:req()}.

reopen_test_run(#empty{}, Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    case db_if_test_runs:status(Id) of
        {ok, closed} ->
            Patch = #{
                <<"status">> => protocol_db:test_run_status_to_json(in_progress),
                <<"started_at">> => util_time:utc_datetime()
            },
            case db_if_test_runs:update(Id, Patch) of
                ok ->
                    case db_if_test_runs:get_one(Id) of
                        {ok, TestRun} ->
                            web_ws:broadcast(#test_run_reopened{actor_id = UserId, actor_name = Username, data = TestRun}),
                            {TestRun, Req};
                        {error, Reason} ->
                            web_rest_test_run_reopen:reopen_test_run_500(#internal_server_error{error = util_binary:to_binary(Reason)})
                    end;
                {error, ?err_not_exists} ->
                    web_rest_test_run_reopen:reopen_test_run_404(#not_found_error{});
                {error, Reason} ->
                    web_rest_test_run_reopen:reopen_test_run_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {ok, _} -> web_rest_test_run_reopen:reopen_test_run_400(#bad_request_error{error = not_closed});
        {error, Reason} -> web_rest_test_run_reopen:reopen_test_run_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec clone_test_run(Request :: protocol_common:empty(), Id :: non_neg_integer(), Req :: cowboy_req:req()) ->
    {protocol_db:test_run(), Req :: cowboy_req:req()}.

clone_test_run(#empty{}, Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    case db_if_test_runs:get_one(Id) of
        {ok, #test_run{project_id = ProjectId, title = _Title}} ->
            % Create test run
            case db_if_test_runs:create(ProjectId, undefined) of
                {ok, TestRunId} ->
                    % Clone test run items
                    case db_if_test_run_items:get_all(Id) of
                        {ok, Items} -> [db_if_test_run_items:create(TestRunId, CaseId, UserId, OrderNum, Params) || #test_run_item{case_id = CaseId, order_num = OrderNum, params = Params} <- Items];
                        _ -> ignore
                    end,

                    % Notify
                    case db_if_test_runs:get_one(TestRunId) of
                        {ok, TestRun} ->
                            web_ws:broadcast(#test_run_created{actor_id = UserId, actor_name = Username, data = TestRun}),
                            {TestRun, Req};
                        {error, Reason} ->
                            web_rest_test_run_clone:clone_test_run_500(#internal_server_error{error = util_binary:to_binary(Reason)})
                    end;
                {error, Reason} ->
                    case is_bad_request(Reason) of
                        true -> web_rest_test_run_clone:clone_test_run_400(#bad_request_error{error = Reason});
                        false -> web_rest_test_run_clone:clone_test_run_500(#internal_server_error{error = util_binary:to_binary(Reason)})
                    end
            end;
        {error, ?err_not_exists} ->
            web_rest_test_run_clone:clone_test_run_404(#not_found_error{});
        {error, Reason} ->
            web_rest_test_run_clone:clone_test_run_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec update_test_run(Request :: protocol_test_run:update_test_run_item_request(), Id :: non_neg_integer(), Req :: cowboy_req:req()) ->
    {protocol_db:test_run(), Req :: cowboy_req:req()}.

update_test_run(#update_test_run_request{} = Request, Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    Patch = protocol_test_run:update_test_run_request_to_json(Request),
    case db_if_test_runs:update(Id, Patch) of
        ok ->
            case db_if_test_runs:get_one(Id) of
                {ok, TestRun} ->
                    web_ws:broadcast(#test_run_updated{actor_id = UserId, actor_name = Username, data = TestRun}),
                    {TestRun, Req};
                {error, Reason} ->
                    web_rest_test_runs:create_test_run_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, Reason} ->
            case is_bad_request(Reason) of
                true -> web_rest_test_runs:create_test_run_400(#bad_request_error{error = Reason});
                false -> web_rest_test_runs:create_test_run_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end
    end.

-spec delete_test_run(Id :: non_neg_integer(), Req :: cowboy_req:req()) ->
    {protocol_data:generic_response(), cowboy_req:req()}.

delete_test_run(Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    case db_if_test_runs:get_one(Id) of
        {ok, TestRun} ->
            ok = db_if_test_run_items:delete_all(Id),
            case db_if_test_runs:delete(Id) of
                ok ->
                    web_ws:broadcast(#test_run_deleted{actor_id = UserId, actor_name = Username, data = TestRun}),
                    {#generic_response{result = true}, Req};
                {error, ?err_not_exists} -> web_rest_test_run:delete_test_run_404(#not_found_error{});
                {error, Reason} -> web_rest_test_run:delete_test_run_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, ?err_not_exists} -> web_rest_test_run:delete_test_run_404(#not_found_error{});
        {error, Reason} -> web_rest_test_run:delete_test_run_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

%% Local functions

is_bad_request(project_not_exists) -> true;
is_bad_request(title_already_exists) -> true;
is_bad_request(_) -> false.
