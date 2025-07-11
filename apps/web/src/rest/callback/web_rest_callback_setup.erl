-module(web_rest_callback_setup).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include_lib("db/include/protocol_common.hrl").
-include_lib("db/include/protocol_db.hrl").
-include("session.hrl").
-include("protocol_data.hrl").
-include("protocol_setup.hrl").
-include("protocol_notification.hrl").

%% Exported functions

-export([
    get_setup/1,
    get_setup_step/1,
    create_setup_step/3,
    update_setup_step/3,
    delete_setup_step/2,
    link_setup_step_to_parameter/4,
    unlink_setup_step_from_parameter/3
]).

%% API

-spec get_setup(ProjectId :: non_neg_integer()) ->
    protocol_data:collection(protocol_data:tree_node(protocol_db:setup_step())).

get_setup(ProjectId) ->
    case db_if_setup_steps:get_all(ProjectId) of
        {ok, SetupSteps} -> #collection{items = list_to_tree(SetupSteps)};
        {error, Reason} -> web_rest_setup:get_setup_500(#internal_server_error{error = Reason})
    end.

-spec get_setup_step(Id :: non_neg_integer()) ->
    protocol_db:setup_step().

get_setup_step(Id) ->
    case db_if_setup_steps:get_one(Id) of
        {ok, SetupStep} -> SetupStep;
        {error, ?err_not_exists} -> web_rest_setup_step:get_setup_step_404(#not_found_error{});
        {error, Reason} -> web_rest_setup_step:get_setup_step_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec create_setup_step(Request, ProjectId, Req) -> Result when
    Request :: protocol_setup:create_setup_step_request(),
    ProjectId :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {protocol_db:setup_step(), cowboy_req:req()}.

create_setup_step(#create_setup_step_request{} = CreateRequest, ProjectId, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    #create_setup_step_request{
        parent_id = ParentId,
        title = Title,
        description = Description,
        is_draft = IsDraft,
        order_num = OrderNum
    } = CreateRequest,
    ParentId1 = ?yesno(ParentId =:= undefined, ?null, ParentId),
    Title1 = ?yesno(Title =:= undefined, ?null, Title),
    Description1 = ?yesno(Description =:= undefined, ?null, Description),
    case db_if_setup_steps:create(ParentId1, ProjectId, Title1, Description1, IsDraft, OrderNum) of
        {ok, SetupStepId} ->
            case db_if_setup_steps:get_one(SetupStepId) of
                {ok, SetupStep} ->
                    web_ws:broadcast(#setup_step_created{actor_id = UserId, actor_name = Username, data = SetupStep}),
                    {SetupStep, Req};
                {error, Reason} ->
                    web_rest_setup_steps:create_setup_step_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, Reason} ->
            case is_bad_request(Reason) of
                true -> web_rest_setup_steps:create_setup_step_400(#bad_request_error{error = Reason});
                false -> web_rest_setup_steps:create_setup_step_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end
    end.

-spec update_setup_step(Request :: protocol_setup:update_setup_step_request(), Id :: non_neg_integer(), Req :: cowboy_req:req()) ->
    {protocol_db:setup_step(), cowboy_req:req()}.

update_setup_step(#update_setup_step_request{} = Request, Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    Patch = protocol_setup:update_setup_step_request_to_json(Request),
    case db_if_setup_steps:update(Id, Patch) of
        ok ->
            case db_if_setup_steps:get_one(Id) of
                {ok, SetupStep} ->
                    web_ws:broadcast(#setup_step_updated{actor_id = UserId, actor_name = Username, data = SetupStep}),
                    {SetupStep, Req};
                {error, Reason} ->
                    web_rest_setup_step:update_setup_step_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, Reason} ->
            case is_bad_request(Reason) of
                true -> web_rest_setup_step:update_setup_step_400(#bad_request_error{error = Reason});
                false -> web_rest_setup_step:update_setup_step_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end
    end.

-spec delete_setup_step(Id :: non_neg_integer(), Req :: cowboy_req:req()) ->
    {protocol_data:generic_response(), cowboy_req:req()}.

delete_setup_step(Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    case db_if_setup_steps:get_one(Id) of
        {ok, SetupStep} ->
            case db_if_setup_steps:delete(Id) of
                ok ->
                    web_ws:broadcast(#setup_step_deleted{actor_id = UserId, actor_name = Username, data = SetupStep}),
                    {#generic_response{result = true}, Req};
                {error, ?err_not_exists} ->
                    web_rest_setup_step:delete_setup_step_404(#not_found_error{});
                {error, Reason} ->
                    case is_bad_request(Reason) of
                        true -> web_rest_setup_step:delete_setup_step_400(#bad_request_error{error = Reason});
                        false -> web_rest_setup_step:delete_setup_step_500(#internal_server_error{error = util_binary:to_binary(Reason)})
                    end
            end;
        {error, ?err_not_exists} -> web_rest_setup_step:delete_setup_step_404(#not_found_error{});
        {error, Reason} -> web_rest_setup_step:delete_setup_step_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec link_setup_step_to_parameter(Request, SetupId, ParamId, Req) -> Result when
    Request :: protocol_common:empty(),
    SetupId :: non_neg_integer(),
    ParamId :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {protocol_db:setup_step(), cowboy_req:req()}.

link_setup_step_to_parameter(#empty{}, SetupId, ParamId, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    case db_if_setup_steps:link_parameter(SetupId, ParamId) of
        ok ->
            case db_if_setup_steps:get_one(SetupId) of
                {ok, SetupStep} ->
                    web_ws:broadcast(#setup_step_updated{actor_id = UserId, actor_name = Username, data = SetupStep}),
                    {SetupStep, Req};
                {error, Reason} ->
                    web_rest_setup_step_param_link:link_setup_step_to_parameter_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, Reason} ->
            IsBadRequest = lists:member(Reason, [
                setup_step_not_exists,
                parameter_not_exists,
                project_mismatch
            ]),
            case IsBadRequest of
                true -> web_rest_setup_step_param_link:link_setup_step_to_parameter_400(#bad_request_error{error = Reason});
                false -> web_rest_setup_step_param_link:link_setup_step_to_parameter_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end
    end.

-spec unlink_setup_step_from_parameter(Request, SetupId, Req) -> Result when
    Request :: protocol_common:empty(),
    SetupId :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {protocol_db:setup_step(), cowboy_req:req()}.

unlink_setup_step_from_parameter(#empty{}, SetupId, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    case db_if_setup_steps:unlink_parameter(SetupId) of
        ok ->
            case db_if_setup_steps:get_one(SetupId) of
                {ok, SetupStep} ->
                    web_ws:broadcast(#setup_step_updated{actor_id = UserId, actor_name = Username, data = SetupStep}),
                    {SetupStep, Req};
                {error, Reason} ->
                    web_rest_setup_step_param_unlink:unlink_setup_step_from_parameter_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, ?err_not_exists} ->
            web_rest_setup_step_param_unlink:unlink_setup_step_from_parameter_404(#not_found_error{});
        {error, Reason} ->
            web_rest_setup_step_param_unlink:unlink_setup_step_from_parameter_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

%% Local functions

is_bad_request(Reason) ->
    lists:member(Reason, [
        parent_not_exists,
        project_not_exists,
        title_already_exists,
        has_children
    ]).

list_to_tree(List) ->
    list_to_tree(List, undefined).

list_to_tree(List, ParentNodeId) ->
    OtherItems = [Item || #setup_step{parent_id = ParentId} = Item <- List, ParentId =/= ParentNodeId],
    [#tree_node{
        item = Item,
        children = list_to_tree(OtherItems, Id)
    } || #setup_step{id = Id, parent_id = ParentId} = Item <- List, ParentId =:= ParentNodeId].
