-module(web_rest_callback_parameter).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include_lib("db/include/protocol_db.hrl").
-include("session.hrl").
-include("protocol_data.hrl").
-include("protocol_parameter.hrl").
-include("protocol_notification.hrl").

%% Exported functions

-export([
    get_parameter/1,
    get_parameters/1,
    create_parameter/3,
    rename_parameter/3,
    delete_parameter/2,
    add_parameter_value/3,
    rename_parameter_value/3,
    remove_parameter_value/3
]).

%% API

-spec get_parameter(Id :: non_neg_integer()) ->
    protocol_db:parameter().

get_parameter(Id) ->
    case db_if_parameters:get_one(Id) of
        {ok, Parameter} -> Parameter;
        {error, ?err_not_exists} -> web_rest_parameter:get_parameter_404(#not_found_error{});
        {error, Reason} -> web_rest_parameter:get_parameter_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec get_parameters(ProjectId :: non_neg_integer()) ->
    protocol_data:collection(protocol_data:tree_node(protocol_db:parameter())).

get_parameters(ProjectId) ->
    case db_if_parameters:get_for_project(ProjectId) of
        {ok, Parameters} -> #collection{items = list_to_tree(Parameters)};
        {error, Reason} -> web_rest_parameters:get_parameters_500(#internal_server_error{error = Reason})
    end.

-spec create_parameter(Request, ProjectId, Req) -> Result when
    Request :: protocol_parameter:create_parameter_request(),
    ProjectId :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {protocol_db:parameter(), cowboy_req:req()}.

create_parameter(#create_parameter_request{} = CreateRequest, ProjectId, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    #create_parameter_request{parent_id = ParentId, dependent_id = DependentId, title = Title} = CreateRequest,
    case db_if_parameters:create(ProjectId, ParentId, DependentId, Title) of
        {ok, ParamId} ->
            case db_if_parameters:get_one(ParamId) of
                {ok, Parameter} ->
                    web_ws:broadcast(#parameter_created{actor_id = UserId, actor_name = Username, data = Parameter}),
                    {Parameter, Req};
                {error, Reason} ->
                    web_rest_parameters:create_parameter_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, Reason} ->
            IsBadRequest = lists:member(Reason, [
                project_not_exists,
                parameter_already_exists,
                parent_not_exists,
                parent_is_independent,
                parameter_source_already_exists
            ]),
            case IsBadRequest of
                true -> web_rest_parameters:create_parameter_400(#bad_request_error{error = Reason});
                false -> web_rest_parameters:create_parameter_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end
    end.

-spec rename_parameter(Request, Id, Req) -> Result when
    Request :: protocol_parameter:rename_parameter_request(),
    Id :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {protocol_db:parameter(), cowboy_req:req()}.

rename_parameter(#rename_parameter_request{new_title = NewTitle}, Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    case db_if_parameters:rename(Id, NewTitle) of
        ok ->
            case db_if_parameters:get_one(Id) of
                {ok, Parameter} ->
                    web_ws:broadcast(#parameter_updated{actor_id = UserId, actor_name = Username, data = Parameter}),
                    {Parameter, Req};
                {error, Reason} ->
                    web_rest_parameter:rename_parameter_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, Reason} ->
            IsBadRequest = lists:member(Reason, [
                parameter_not_exists,
                title_already_exists
            ]),
            case IsBadRequest of
                true -> web_rest_parameter:rename_parameter_400(#bad_request_error{error = Reason});
                false -> web_rest_parameter:rename_parameter_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end
    end.

-spec delete_parameter(Id, Req) -> Result when
    Id :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {protocol_data:generic_response(), cowboy_req:req()}.

delete_parameter(Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    case db_if_parameters:get_one(Id) of
        {ok, Parameter} ->
            case db_if_parameters:delete(Id) of
                ok ->
                    web_ws:broadcast(#parameter_deleted{actor_id = UserId, actor_name = Username, data = Parameter}),
                    {#generic_response{result = true}, Req};
                {error, Reason} ->
                    IsBadRequest = lists:member(Reason, [
                        parameter_not_exists,
                        has_children,
                        has_dependants
                    ]),
                    case IsBadRequest of
                        true -> web_rest_parameter:delete_parameter_400(#bad_request_error{error = Reason});
                        false -> web_rest_parameter:delete_parameter_500(#internal_server_error{error = util_binary:to_binary(Reason)})
                    end
            end;
        {error, ?err_not_exists} -> web_rest_parameter:delete_parameter_404(#not_found_error{});
        {error, Reason} -> web_rest_parameter:delete_parameter_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec add_parameter_value(Request, Id, Req) -> Result when
    Request :: protocol_parameter:add_parameter_value_request(),
    Id :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {protocol_db:parameter(), cowboy_req:req()}.

add_parameter_value(#add_parameter_value_request{} = Request, Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    #add_parameter_value_request{dependent_value = DependentValue, value = Value} = Request,
    case db_if_parameters:add_value(Id, DependentValue, Value) of
        ok ->
            case db_if_parameters:get_one(Id) of
                {ok, Parameter} ->
                    web_ws:broadcast(#parameter_updated{actor_id = UserId, actor_name = Username, data = Parameter}),
                    {Parameter, Req};
                {error, Reason} ->
                    web_rest_parameter_value_add:add_parameter_value_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, Reason} ->
            IsBadRequest = lists:member(Reason, [
                parameter_not_exists,
                parameter_source_not_exists,
                value_already_exists,
                value_not_exists,
                dependent_parameter_not_exists,
                dependent_source_not_exists,
                dependent_value_not_exists
            ]),
            case IsBadRequest of
                true -> web_rest_parameter_value_add:add_parameter_value_400(#bad_request_error{error = Reason});
                false -> web_rest_parameter_value_add:add_parameter_value_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end
    end.

-spec rename_parameter_value(Request, Id, Req) -> Result when
    Request :: protocol_parameter:rename_parameter_value_request(),
    Id :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {protocol_db:parameter(), cowboy_req:req()}.

rename_parameter_value(#rename_parameter_value_request{} = Request, Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    #rename_parameter_value_request{old_value = OldValue, new_value = NewValue} = Request,
    case db_if_parameters:rename_value(Id, OldValue, NewValue) of
        ok ->
            case db_if_parameters:get_one(Id) of
                {ok, Parameter} ->
                    web_ws:broadcast(#parameter_updated{actor_id = UserId, actor_name = Username, data = Parameter}),
                    {Parameter, Req};
                {error, Reason} ->
                    web_rest_parameter_value_rename:rename_parameter_value_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, Reason} ->
            IsBadRequest = lists:member(Reason, [
                parameter_not_exists,
                invalid_parameter,
                value_not_exists,
                value_already_exists
            ]),
            case IsBadRequest of
                true -> web_rest_parameter_value_rename:rename_parameter_value_400(#bad_request_error{error = Reason});
                false -> web_rest_parameter_value_rename:rename_parameter_value_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end
    end.

-spec remove_parameter_value(Request, Id, Req) -> Result when
    Request :: protocol_parameter:remove_parameter_value_request(),
    Id :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {protocol_db:parameter(), cowboy_req:req()}.

remove_parameter_value(#remove_parameter_value_request{} = Request, Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    #remove_parameter_value_request{dependent_value = DependentValue, value = Value} = Request,
    case db_if_parameters:remove_value(Id, DependentValue, Value) of
        ok ->
            case db_if_parameters:get_one(Id) of
                {ok, Parameter} ->
                    web_ws:broadcast(#parameter_updated{actor_id = UserId, actor_name = Username, data = Parameter}),
                    {Parameter, Req};
                {error, Reason} ->
                    web_rest_parameter_value_remove:remove_parameter_value_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, Reason} ->
            IsBadRequest = lists:member(Reason, [
                parameter_not_exists,
                value_not_exists,
                linked_to_children,
                linked_to_dependants,
                dependent_parameter_not_exists,
                dependent_source_not_exists,
                dependent_value_not_exists
            ]),
            case IsBadRequest of
                true -> web_rest_parameter_value_remove:remove_parameter_value_400(#bad_request_error{error = Reason});
                false -> web_rest_parameter_value_remove:remove_parameter_value_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end
    end.

%% Local functions

list_to_tree(List) ->
    list_to_tree(List, undefined).

list_to_tree(List, ParentNodeId) ->
    OtherItems = [Item || #parameter{parent_id = ParentId} = Item <- List, ParentId =/= ParentNodeId],
    [#tree_node{
        item = Item,
        children = list_to_tree(OtherItems, Id)
    } || #parameter{id = Id, parent_id = ParentId} = Item <- List, ParentId =:= ParentNodeId].
