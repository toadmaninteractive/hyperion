-module(web_rest_callback_jira).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include_lib("db/include/protocol_common.hrl").
-include_lib("db/include/protocol_db.hrl").
-include_lib("jira/include/jira_protocol.hrl").
-include("session.hrl").
-include("protocol_data.hrl").
-include("protocol_jira.hrl").
-include("protocol_notification.hrl").

%% Exported functions

-export([
    % JIRA instances
    get_jira_instance/1,
    get_jira_instances/0,
    create_jira_instance/2,
    update_jira_instance/3,
    delete_jira_instance/2,

    % JIRA authentication
    get_authentication_status/2,
    authenticate/3,
    revoke_authentication/2,

    % JIRA issues
    create_jira_issue/3
]).

%% API

-spec get_jira_instance(Id) -> Result when
    Id :: non_neg_integer(),
    Result :: protocol_db:jira_instance().

get_jira_instance(Id) ->
    case db_if_jira_instances:get_one(Id) of
        {ok, JiraInstance} -> JiraInstance;
        {error, ?err_not_exists} -> web_rest_jira_instance:get_jira_instance_404(#not_found_error{});
        {error, Reason} -> web_rest_jira_instance:get_jira_instance_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec get_jira_instances() ->
    protocol_data:collection(protocol_db:jira_instance()).

get_jira_instances() ->
    case db_if_jira_instances:get_all() of
        {ok, JiraInstances} -> #collection{items = JiraInstances};
        {error, Reason} -> web_rest_jira_instances:get_jira_instances_500(#internal_server_error{error = Reason})
    end.

-spec create_jira_instance(Request, Req) -> Result when
    Request :: protocol_jira:create_jira_instance_request(), Req :: cowboy_req:req(),
    Result :: {protocol_db:jira_instance(), cowboy_req:req()}.

create_jira_instance(Request, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    #create_jira_instance_request{title = Title, url = Url} = Request,
    case db_if_jira_instances:create(Title, Url) of
        {ok, JiraId} ->
            case db_if_jira_instances:get_one(JiraId) of
                {ok, JiraInstance} ->
                    web_ws:broadcast(#jira_instance_created{actor_id = UserId, actor_name = Username, data = JiraInstance}),
                    {JiraInstance, Req};
                {error, Reason} ->
                    web_rest_jira_instances:create_jira_instance_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, Reason} ->
            case is_bad_request(Reason) of
                true -> web_rest_jira_instances:create_jira_instance_400(#bad_request_error{error = Reason});
                false -> web_rest_jira_instances:create_jira_instance_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end
    end.

-spec update_jira_instance(Request, Id, Req) -> Result when
    Request :: protocol_jira:update_jira_instance_request(),
    Id :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {protocol_db:jira_instance(), cowboy_req:req()}.

update_jira_instance(#update_jira_instance_request{} = Request, Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    Patch = protocol_jira:update_jira_instance_request_to_json(Request),
    case db_if_jira_instances:update(Id, Patch) of
        ok ->
            case db_if_jira_instances:get_one(Id) of
                {ok, JiraInstance} ->
                    web_ws:broadcast(#jira_instance_updated{actor_id = UserId, actor_name = Username, data = JiraInstance}),
                    {JiraInstance, Req};
                {error, Reason} ->
                    web_rest_jira_instance:update_jira_instance_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, Reason} ->
            case is_bad_request(Reason) of
                true -> web_rest_jira_instance:update_jira_instance_400(#bad_request_error{error = Reason});
                false -> web_rest_jira_instance:update_jira_instance_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end
    end.

-spec delete_jira_instance(Id, Req) -> Result when
    Id :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {protocol_data:generic_response(), cowboy_req:req()}.

delete_jira_instance(Id, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    case db_if_jira_instances:get_one(Id) of
        {ok, JiraInstance} ->
            case db_if_jira_instances:delete(Id) of
                {ok, Result} ->
                    ?doif(Result, web_ws:broadcast(#jira_instance_deleted{actor_id = UserId, actor_name = Username, data = JiraInstance})),
                    {#generic_response{result = Result}, Req};
                {error, ?err_not_exists} -> web_rest_jira_instance:delete_jira_instance_404(#not_found_error{});
                {error, Reason} -> web_rest_jira_instance:delete_jira_instance_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, ?err_not_exists} -> web_rest_jira_instance:delete_jira_instance_404(#not_found_error{});
        {error, Reason} -> web_rest_jira_instance:delete_jira_instance_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec get_authentication_status(JiraId, Req) -> Result when
    JiraId :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {protocol_data:generic_response(), cowboy_req:req()}.

get_authentication_status(JiraId, #{?m_session := #session{user_id = UserId, username = _Username}} = Req) ->
    case db_if_jira_instances:exists(JiraId) of
        {ok, true} -> ignore;
        {ok, false} -> web_rest_jira_auth:get_authentication_status_404(#not_found_error{});
        {error, Reason} -> web_rest_jira_auth:get_authentication_status_500(#internal_server_error{error = Reason})
    end,
    case db_if_jira_auth:exists(UserId, JiraId) of
        {ok, Result} -> {#generic_response{result = Result}, Req};
        {error, Reason1} -> web_rest_jira_auth:get_authentication_status_500(#internal_server_error{error = Reason1})
    end.

-spec authenticate(Request, JiraId, Req) -> Result when
    Request :: protocol_jira:authenticate_jira_request(),
    JiraId :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {protocol_data:generic_response(), cowboy_req:req()}.

authenticate(Request, JiraId, #{?m_session := #session{user_id = UserId, username = _Username}} = Req) ->
    #authenticate_jira_request{username = JiraUsername, password = Password} = Request,
    BaseUrl = case db_if_jira_instances:get_one(JiraId) of
        {ok, #jira_instance{url = Url}} -> Url;
        {error, ?err_not_exists} -> web_rest_jira_auth:authenticate_404(#not_found_error{});
        {error, Reason} -> web_rest_jira_auth:authenticate_500(#internal_server_error{error = Reason})
    end,
    AuthToken = base64:encode(<<JiraUsername/binary, ":", Password/binary>>),
    try
        jira:get_user(BaseUrl, AuthToken, JiraUsername)
    catch
        error:{http_error, 401, _}:_ -> web_rest_jira_auth:authenticate_400(#bad_request_error{error = jira_user_not_exists});
        error:{http_error, 403, _}:_ -> web_rest_jira_auth:authenticate_400(#bad_request_error{error = invalid_credentials});
        error:badarg:_ -> web_rest_jira_auth:authenticate_400(#bad_request_error{error = invalid_jira_url})
    end,
    ok = db_if_jira_auth:set(UserId, JiraId, JiraUsername, encode_auth_token(AuthToken)),
    {#generic_response{result = true}, Req}.

-spec revoke_authentication(JiraId, Req) -> Result when
    JiraId :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {protocol_data:generic_response(), cowboy_req:req()}.

revoke_authentication(JiraId, #{?m_session := #session{user_id = UserId, username = _Username}} = Req) ->
    case db_if_jira_auth:delete(UserId, JiraId) of
        {ok, Result} -> {#generic_response{result = Result}, Req};
        {error, ?err_not_exists} -> web_rest_jira_auth:revoke_authentication_404(#not_found_error{});
        {error, Reason} -> web_rest_jira_auth:revoke_authentication_500(#internal_server_error{error = Reason})
    end.

-spec create_jira_issue(Request, TestRunItemId, Req) -> Result when
    Request :: protocol_jira:create_jira_issue_request(),
    TestRunItemId :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {protocol_jira:create_jira_issue_response(), cowboy_req:req()}.

create_jira_issue(Request, TestRunItemId, #{?m_session := #session{user_id = UserId, username = _Username}} = Req) ->
    % Check if JIRA issue already assigned
    case db_if_test_run_items:jira_issue_key(TestRunItemId) of
        {ok, JIK} when is_binary(JIK) -> web_rest_jira_issues:create_jira_issue_400(#bad_request_error{error = jira_issue_already_assigned});
        _ -> ignore
    end,

    % Check if issue summary is not an empty string
    #create_jira_issue_request{summary = Summary, description = Description} = Request,
    Summary1 = util_binary:trim(Summary),
    ?doif(Summary1 =:= <<>>, web_rest_jira_issues:create_jira_issue_400(#bad_request_error{error = invalid_summary})),

    % Get test run ID
    TestRunId = case db_if_test_run_items:run_id(TestRunItemId) of
        {ok, RID} -> RID;
        {error, ?err_not_exists} -> web_rest_jira_issues:create_jira_issue_404(#not_found_error{});
        {error, Reason} -> web_rest_jira_issues:create_jira_issue_500(#internal_server_error{error = Reason})
    end,

    % Get project ID
    ProjectId = case db_if_test_runs:project_id(TestRunId) of
        {ok, PID} -> PID;
        {error, ?err_not_exists} -> web_rest_jira_issues:create_jira_issue_400(#bad_request_error{error = invalid_project});
        {error, Reason1} -> web_rest_jira_issues:create_jira_issue_500(#internal_server_error{error = Reason1})
    end,

    % Get JIRA ID and key
    {JiraId, JiraKey} = case db_if_projects:get_one(ProjectId) of
        {ok, #project{jira_id = JID, jira_key = Key}} -> {JID, Key};
        {error, ?err_not_exists} -> web_rest_jira_issues:create_jira_issue_400(#bad_request_error{error = invalid_project});
        {error, Reason2} -> web_rest_jira_issues:create_jira_issue_500(#internal_server_error{error = Reason2})
    end,

    % Check if project is linked to JIRA
    ?doif(JiraId =:= undefined, web_rest_jira_issues:create_jira_issue_400(#bad_request_error{error = jira_instance_not_linked})),

    % Check if JIRA project key is not an empty string
    JiraKey1 = ?yesno(is_binary(JiraKey), util_binary:trim(JiraKey), <<>>),
    ?doif(JiraKey1 =:= <<>>, web_rest_jira_issues:create_jira_issue_400(#bad_request_error{error = invalid_jira_key})),

    % Get JIRA base URL
    BaseUrl = case db_if_jira_instances:get_one(JiraId) of
        {ok, #jira_instance{url = Url}} -> Url;
        {error, ?err_not_exists} -> web_rest_jira_issues:create_jira_issue_400(#bad_request_error{error = invalid_jira_instance});
        {error, Reason3} -> web_rest_jira_issues:create_jira_issue_500(#internal_server_error{error = Reason3})
    end,

    % Get JIRA authentication token
    EncodedAuthToken = case db_if_jira_auth:get_one(UserId, JiraId) of
        {ok, #jira_auth{auth_token = EAT}} -> EAT;
        {error, ?err_not_exists} -> web_rest_jira_issues:create_jira_issue_400(#bad_request_error{error = no_credentials});
        {error, Reason4} -> web_rest_jira_issues:create_jira_issue_500(#internal_server_error{error = Reason4})
    end,

    % Decode authentication token
    AuthToken = decode_auth_token(EncodedAuthToken),

    % Try to create an issue in JIRA
    #create_issue_response{key = JiraIssueKey} = try
        jira:create_issue(BaseUrl, AuthToken, JiraKey1, bug, Summary1, Description)
    catch
        error:{http_error, 401, _}:_ -> web_rest_jira_issues:create_jira_issue_400(#bad_request_error{error = jira_user_not_exists});
        error:{http_error, 403, _}:_ -> web_rest_jira_issues:create_jira_issue_400(#bad_request_error{error = invalid_credentials});
        error:badarg:_ -> web_rest_jira_issues:create_jira_issue_500(#bad_request_error{error = invalid_jira_url})
    end,

    % Assign JIRA issue to test run item
    ok = db_if_test_run_items:update(TestRunItemId, #{<<"jira_issue_key">> => JiraIssueKey}),
    {ok, #test_run_item{jira_issue_url = JiraIssueUrl}} = db_if_test_run_items:get_one(TestRunItemId),
    {#create_jira_issue_response{jira_issue_key = JiraIssueKey, jira_issue_url = JiraIssueUrl}, Req}.

%% Local functions

is_bad_request(Reason) ->
    lists:member(Reason, [
        title_already_exists,
        invalid_title,
        invalid_url
    ]).

encode_auth_token(AuthToken) ->
    jws:encode_compact(AuthToken, #{alg => <<"RS256">>}, web_storage:jwk()).

decode_auth_token(EncodedAuthToken) ->
    try
        {true, AuthToken, _} = jws:decode_compact(EncodedAuthToken, web_storage:jwk()),
        AuthToken
    catch
        _C:_R:_S -> undefined
    end.
