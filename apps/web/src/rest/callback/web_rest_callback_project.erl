-module(web_rest_callback_project).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include_lib("db/include/protocol_db.hrl").
-include("session.hrl").
-include("protocol_data.hrl").
-include("protocol_project.hrl").
-include("protocol_notification.hrl").

%% Exported functions

-export([
    get_project/1,
    get_projects/1,
    create_project/2,
    update_project/3,
    delete_project/2
]).

%% API

-spec get_project(Id :: non_neg_integer()) ->
    protocol_db:project().

get_project(Id) ->
    case db_if_projects:get_one(Id) of
        {ok, Project} -> Project;
        {error, ?err_not_exists} -> web_rest_project:get_project_404(#not_found_error{});
        {error, Reason} -> web_rest_project:get_project_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec get_projects(cowboy_req:req()) ->
    protocol_data:collection(protocol_db:project()).

get_projects(#{?m_session := #session{user_id = UserId}} = Req) ->
    Result = ?yesno(access_config:local_admin(), db_if_projects:get_all(), db_if_projects:get_accessible(UserId)),
    case Result of
        {ok, Projects} -> {#collection{items = Projects}, Req};
        {error, Reason} -> web_rest_projects:get_projects_500(#internal_server_error{error = Reason})
    end.

-spec create_project(Request :: protocol_project:create_project_request(), Req :: cowboy_req:req()) ->
    {protocol_db:project(), cowboy_req:req()}.

create_project(Request, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    #create_project_request{
        title = Title,
        key = Key,
        jira_id = JiraId,
        jira_key = JiraKey,
        slack_receivers = SlackReceivers
    } = Request,
    SlackReceivers1 = unique_receivers(SlackReceivers),
    case db_if_projects:create(Title, Key, JiraId, JiraKey, SlackReceivers1, UserId) of
        {ok, ProjectId} ->
            case db_if_projects:get_one(ProjectId) of
                {ok, Project} ->
                    web_ws:broadcast(#project_created{actor_id = UserId, actor_name = Username, data = Project}),
                    {Project, Req};
                {error, Reason} ->
                    web_rest_projects:create_project_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, Reason} ->
            case is_bad_request(Reason) of
                true -> web_rest_projects:create_project_400(#bad_request_error{error = Reason});
                false -> web_rest_projects:create_project_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end
    end.

-spec update_project(Request :: protocol_project:update_project_request(), Id :: non_neg_integer(), Req :: cowboy_req:req()) ->
    {protocol_db:project(), cowboy_req:req()}.

update_project(#update_project_request{slack_receivers = SlackReceivers} = Request, Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    Request1 = Request#update_project_request{slack_receivers = unique_receivers(SlackReceivers)},
    Patch = protocol_project:update_project_request_to_json(Request1),
    case db_if_projects:update(Id, Patch) of
        ok ->
            case db_if_projects:get_one(Id) of
                {ok, Project} ->
                    web_ws:broadcast(#project_updated{actor_id = UserId, actor_name = Username, data = Project}),
                    {Project, Req};
                {error, Reason} ->
                    web_rest_project:update_project_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, Reason} ->
            case is_bad_request(Reason) of
                true -> web_rest_project:update_project_400(#bad_request_error{error = Reason});
                false -> web_rest_project:update_project_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end
    end.

-spec delete_project(Id :: non_neg_integer(), Req :: cowboy_req:req()) ->
    {protocol_data:generic_response(), cowboy_req:req()}.

delete_project(Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    case db_if_projects:get_one(Id) of
        {ok, Project} ->
            case db_if_projects:delete(Id) of
                ok ->
                    web_ws:broadcast(#project_deleted{actor_id = UserId, actor_name = Username, data = Project}),
                    {#generic_response{result = true}, Req};
                {error, ?err_not_exists} -> web_rest_project:delete_project_404(#not_found_error{});
                {error, Reason} -> web_rest_project:delete_project_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, ?err_not_exists} -> web_rest_project:delete_project_404(#not_found_error{});
        {error, Reason} -> web_rest_project:delete_project_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

%% Local functions

is_bad_request(Reason) ->
    lists:member(Reason, [
        invalid_title,
        title_already_exists,
        invalid_key,
        key_already_exists,
        owner_not_exists
    ]).

unique_receivers(SlackReceivers) when is_binary(SlackReceivers) ->
    SlackReceivers1 = [util_binary:trim(SR) || SR <- binary:split(SlackReceivers, <<",">>, [global])],
    SlackReceivers2 =[util_binary:to_lower(SR) || SR <- SlackReceivers1, SR =/= <<>>],
    util_binary:join(SlackReceivers2, <<",">>);
unique_receivers(SlackReceivers) ->
    SlackReceivers.
