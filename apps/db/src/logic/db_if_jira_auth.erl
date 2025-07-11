-module(db_if_jira_auth).

%% Include files

-include_lib("epgsql/include/epgsql.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/2,
    set/4,
    delete/2,
    exists/2
]).

%% API

-spec get_one(PersonnelId, JiraId) -> Result when
    PersonnelId :: non_neg_integer(),
    JiraId :: non_neg_integer(),
    Result :: {'ok', protocol_db:jira_instance()} | {'error', Reason :: atom()}.

get_one(PersonnelId, JiraId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM jira_auth AS ja ",
        (common_joins())/binary,
        "WHERE ja.personnel_id = $1 AND ja.jira_id = $2"
    >>,
    case db_query:select_one(Query, [PersonnelId, JiraId]) of
        {ok, Item} -> {ok, protocol_db:jira_auth_from_json(Item)};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec set(PersonnelId, JiraId, Username, AuthToken) -> Result when
    PersonnelId :: non_neg_integer(),
    JiraId :: non_neg_integer(),
    Username :: binary(),
    AuthToken :: binary(),
    Result :: 'ok' | {'error', Reason :: atom()}.

set(PersonnelId, JiraId, Username, AuthToken) ->
    Query = <<
        "INSERT INTO jira_auth (personnel_id, jira_id, username, auth_token) ",
        "VALUES ($1, $2, TRIM($3), TRIM($4)) ",
        "ON CONFLICT (\"personnel_id\", \"jira_id\") DO UPDATE SET ",
            "username = TRIM(EXCLUDED.username), "
            "auth_token = TRIM(EXCLUDED.auth_token) "
    >>,
    Params = [PersonnelId, JiraId, Username, AuthToken],
    case db_query:insert(Query, Params, [raw_error]) of
        {ok, 1} -> ok;
        {error, #error{codename = check_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"jira_auth_username_check">> -> {error, invalid_username};
                <<"jira_auth_auth_token_check">> -> {error, invalid_auth_token}
            end;
        {error, #error{codename = foreign_key_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"jira_auth_personnel_id_fkey">> -> {error, personnel_not_exists};
                <<"jira_auth_jira_id_fkey">> -> {error, jira_not_exists}
            end;
        {error, #error{codename = Code}} -> {error, Code}
    end.

-spec delete(PersonnelId, JiraId) -> Result when
    PersonnelId :: non_neg_integer(),
    JiraId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete(PersonnelId, JiraId) ->
    Query = <<"DELETE FROM jira_auth WHERE personnel_id = $1 AND jira_id = $2">>,
    case db_query:delete(Query, [PersonnelId, JiraId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec exists(PersonnelId, JiraId) -> Result when
    PersonnelId :: non_neg_integer(),
    JiraId :: non_neg_integer(),
    Result :: {'ok', boolean()} | {'error', Reason :: atom()}.

exists(PersonnelId, JiraId) ->
    Query = <<"SELECT COUNT(*)::bigint > 0 AS result FROM jira_auth WHERE personnel_id = $1 AND jira_id = $2">>,
    case db_query:select_one(Query, [PersonnelId, JiraId]) of
        {ok, #{<<"result">> := Result}} -> {ok, Result};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % ja : jira_auth
    <<
        "ja.personnel_id AS personnel_id, ",
        "ja.jira_id AS jira_id, ",
        "ja.username AS username, ",
        "ja.auth_token AS auth_token, ",
        "ja.created_at AS created_at, ",
        "ja.updated_at AS updated_at "
    >>.

common_joins() ->
    <<>>.
