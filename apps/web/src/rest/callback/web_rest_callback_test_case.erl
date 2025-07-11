-module(web_rest_callback_test_case).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include_lib("db/include/protocol_common.hrl").
-include_lib("db/include/protocol_db.hrl").
-include("session.hrl").
-include("protocol_data.hrl").
-include("protocol_test_case.hrl").
-include("protocol_notification.hrl").

%% Exported functions

-export([
    get_test_case_tree/1,
    get_test_case/1,
    create_test_case/3,
    update_test_case/3,
    delete_test_case/2,
    specialize_test_case/4,
    despecialize_test_case/4
]).

%% API

-spec get_test_case_tree(ProjectId :: non_neg_integer()) ->
    protocol_data:collection(protocol_data:tree_node(protocol_db:test_case())).

get_test_case_tree(ProjectId) ->
    case db_if_test_cases:get_all(ProjectId) of
        {ok, TestCases} -> #collection{items = list_to_tree(TestCases)};
        {error, Reason} -> web_rest_test_case_tree:get_test_case_tree_500(#internal_server_error{error = Reason})
    end.

-spec get_test_case(Id :: non_neg_integer()) ->
    protocol_db:test_case().

get_test_case(Id) ->
    case db_if_test_cases:get_one(Id) of
        {ok, TestCase} -> TestCase;
        {error, ?err_not_exists} -> web_rest_test_case:get_test_case_404(#not_found_error{});
        {error, Reason} -> web_rest_test_case:get_test_case_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec create_test_case(Request, ProjectId, Req) -> Result when
    Request :: protocol_test_case:create_test_case_request(),
    ProjectId :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {protocol_db:test_case(), cowboy_req:req()}.

create_test_case(#create_test_case_request{} = Request, ProjectId, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    % Deconstruct request
    #create_test_case_request{
        parent_id = ParentId,
        precondition_id = PreconditionId,
        setup_id = SetupId,
        is_group = IsGroup,
        title = Title,
        description = Description,
        test_steps = TestSteps,
        expected_result = ExpectedResult,
        is_draft = IsDraft,
        order_num = OrderNum
    } = Request,

    % Prepare nullable fields
    ParentId1 = ?yesno(ParentId =:= undefined, ?null, ParentId),
    PreconditionId1 = ?yesno(SetupId =:= undefined, ?null, PreconditionId),
    SetupId1 = ?yesno(SetupId =:= undefined, ?null, SetupId),
    Description1 = ?yesno(Description =:= undefined, ?null, Description),

    case db_if_test_cases:create(ParentId1, ProjectId, PreconditionId1, SetupId1, IsGroup, Title, Description1, TestSteps, ExpectedResult, IsDraft, OrderNum) of
        {ok, CaseId} ->
            case db_if_test_cases:get_one(CaseId) of
                {ok, TestCase} ->
                    web_ws:broadcast(#test_case_created{actor_id = UserId, actor_name = Username, data = TestCase}),
                    {TestCase, Req};
                {error, Reason} ->
                    web_rest_test_cases:create_test_case_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, Reason} ->
            case is_bad_request(Reason) of
                true -> web_rest_test_cases:create_test_case_400(#bad_request_error{error = Reason});
                false -> web_rest_test_cases:create_test_case_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end
    end.

-spec update_test_case(Request :: protocol_test_case:update_test_case_request(), Id :: non_neg_integer(), Req :: cowboy_req:req()) ->
    {protocol_db:test_case(), cowboy_req:req()}.

update_test_case(#update_test_case_request{} = Request, Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    Patch = protocol_test_case:update_test_case_request_to_json(Request),
    case db_if_test_cases:update(Id, Patch) of
        ok ->
            case db_if_test_cases:get_one(Id) of
                {ok, TestCase} ->
                    web_ws:broadcast(#test_case_updated{actor_id = UserId, actor_name = Username, data = TestCase}),
                    {TestCase, Req};
                {error, Reason} ->
                    web_rest_test_case:update_test_case_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, Reason} ->
            case is_bad_request(Reason) of
                true -> web_rest_test_case:update_test_case_400(#bad_request_error{error = Reason});
                false -> web_rest_test_case:update_test_case_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end
    end.

-spec delete_test_case(Id :: non_neg_integer(), Req :: cowboy_req:req()) ->
    {protocol_data:generic_response(), cowboy_req:req()}.

delete_test_case(Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    case db_if_test_cases:get_one(Id) of
        {ok, TestCase} ->
            case db_if_test_cases:delete(Id) of
                ok ->
                    web_ws:broadcast(#test_case_deleted{actor_id = UserId, actor_name = Username, data = TestCase}),
                    {#generic_response{result = true}, Req};
                {error, ?err_not_exists} ->
                    web_rest_test_case:delete_test_case_404(#not_found_error{});
                {error, Reason} ->
                    case is_bad_request(Reason) of
                        true -> web_rest_test_case:delete_test_case_400(#bad_request_error{error = Reason});
                        false -> web_rest_test_case:delete_test_case_500(#internal_server_error{error = util_binary:to_binary(Reason)})
                    end
            end;
        {error, ?err_not_exists} -> web_rest_test_case:delete_test_case_404(#not_found_error{});
        {error, Reason} -> web_rest_test_case:delete_test_case_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec specialize_test_case(Request, CaseId, SetupId, Req) -> Result when
    Request :: protocol_test_case:specialize_test_case_request(),
    CaseId :: non_neg_integer(),
    SetupId :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {protocol_db:test_case(), cowboy_req:req()}.

specialize_test_case(#specialize_test_case_request{} = Request, CaseId, SetupId, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    #specialize_test_case_request{spec_type = SpecType, value = Value, parameter_id = ParameterId} = Request,
    Id = case SpecType of
        value -> parameter_value_id(SetupId, Value);
        source -> ParameterId;
        _ -> undefined
    end,
    case db_if_test_cases:specialize(CaseId, SetupId, SpecType, Id) of
        ok ->
            case db_if_test_cases:get_one(CaseId) of
                {ok, TestCase} ->
                    web_ws:broadcast(#test_case_updated{actor_id = UserId, actor_name = Username, data = TestCase}),
                    {TestCase, Req};
                {error, Reason} ->
                    web_rest_test_case_specialize:specialize_test_case_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, Reason} ->
            IsBadRequest = lists:member(Reason, [
                test_case_not_exists,
                setup_step_not_assigned,
                setup_step_not_exists,
                invalid_setup_step,
                parameterless_setup_step,
                invalid_parameter_value,
                invalid_parameter_source
            ]),
            case IsBadRequest of
                true -> web_rest_test_case_specialize:specialize_test_case_400(#bad_request_error{error = Reason});
                false -> web_rest_test_case_specialize:specialize_test_case_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end
    end.

-spec despecialize_test_case(Request, CaseId, SetupId, Req) -> Result when
    Request :: protocol_common:empty(),
    CaseId :: non_neg_integer(),
    SetupId :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {protocol_db:test_case(), cowboy_req:req()}.

despecialize_test_case(#empty{}, CaseId, SetupId, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    case db_if_test_cases:despecialize(CaseId, SetupId) of
        ok ->
            case db_if_test_cases:get_one(CaseId) of
                {ok, TestCase} ->
                    web_ws:broadcast(#test_case_updated{actor_id = UserId, actor_name = Username, data = TestCase}),
                    {TestCase, Req};
                {error, Reason} ->
                    web_rest_test_case_despecialize:despecialize_test_case_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, ?err_not_exists} ->
            web_rest_test_case_despecialize:despecialize_test_case_404(#not_found_error{});
        {error, Reason} ->
            web_rest_test_case_despecialize:despecialize_test_case_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

%% Local functions

is_bad_request(Reason) ->
    lists:member(Reason, [
        parent_not_exists,
        project_not_exists,
        precondition_not_exists,
        setup_not_exists,
        title_already_exists,
        has_children
    ]).

list_to_tree(List) ->
    list_to_tree(List, undefined).

list_to_tree(List, ParentNodeId) ->
    OtherItems = [Item || #test_case{parent_id = ParentId} = Item <- List, ParentId =/= ParentNodeId],
    [#tree_node{
        item = Item,
        children = list_to_tree(OtherItems, Id)
    } || #test_case{id = Id, parent_id = ParentId} = Item <- List, ParentId =:= ParentNodeId].

parameter_value_id(SetupId, Value) ->
    try
        {ok, ParamId} = db_if_setup_steps:linked_parameter_id(SetupId),
        {ok, ValueId} = db_if_parameters:value_id(ParamId, Value),
        ValueId
    catch
        _:_:_ -> undefined
    end.
