-module(db_if_projects).

%% Include files

-include_lib("epgsql/include/epgsql.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/1,
    get_all/0,
    get_accessible/1,
    create/6,
    update/2,
    update/3,
    delete/1,
    exists/1,
    is_accessible/2
]).

%% API

-spec get_one(ProjectId) -> Result when
    ProjectId :: non_neg_integer(),
    Result :: {'ok', protocol_db:project()} | {'error', Reason :: atom()}.

get_one(ProjectId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM projects AS pr ",
        (common_joins())/binary,
        "WHERE pr.id = $1"
    >>,
    case db_query:select_one(Query, [ProjectId]) of
        {ok, Item} -> {ok, protocol_db:project_from_json(Item)};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_all() ->
    {'ok', Items :: [protocol_db:project()]} | {'error', Reason :: atom()}.

get_all() ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM projects AS pr ",
        (common_joins())/binary,
        "ORDER BY LOWER(pr.title) ASC"
    >>,
    case db_query:select(Query, []) of
        {ok, Items} -> {ok, [protocol_db:project_from_json(Item) || Item <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec get_accessible(PersonnelId :: non_neg_integer()) ->
    {'ok', Items :: [protocol_db:project()]} | {'error', Reason :: atom()}.

get_accessible(PersonnelId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM projects AS pr ",
        (common_joins())/binary,
        "WHERE is_project_accessible_by_personnel(pr.id, $1) ",
        "ORDER BY pr.title ASC"
    >>,
    case db_query:select(Query, [PersonnelId]) of
        {ok, Items} -> {ok, [protocol_db:project_from_json(Item) || Item <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec create(Title, Key, JiraId, JiraKey, SlackReceivers, OwnerId) -> Result when
    Title :: binary(),
    Key :: binary(),
    JiraId :: non_neg_integer() | 'undefined',
    JiraKey :: binary() | 'undefined',
    SlackReceivers :: binary() | 'undefined',
    OwnerId :: non_neg_integer(),
    Result :: {'ok', ProjectId :: non_neg_integer()} | {'error', Reason :: atom()}.

create(Title, Key, JiraId, JiraKey, SlackReceivers, OwnerId) ->
    Query = <<
        "INSERT INTO projects (title, \"key\", jira_id, jira_key, slack_receivers, owner_id) ",
        "VALUES (TRIM($1), TRIM($2), $3, TRIM($4), TRIM($5), $6) ",
        "RETURNING id::bigint"
    >>,
    Params = [Title, Key, JiraId, JiraKey, SlackReceivers, OwnerId],
    case db_query:insert(Query, Params, [raw_error]) of
        {ok, 1, Columns, Rows} ->
            [#{<<"id">> := ProjectId}] = db_util:result_to_json(Columns, Rows),
            {ok, ProjectId};
        {error, #error{codename = check_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"projects_title_check">> -> {error, invalid_title};
                <<"projects_key_check">> -> {error, invalid_key};
                <<"projects_slack_receivers_check">> -> {error, invalid_slack_receivers}
            end;
        {error, #error{codename = unique_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"projects_title_key">> -> {error, title_already_exists};
                <<"proj_title_ult_index">> -> {error, title_already_exists};
                <<"projects_key_key">> -> {error, key_already_exists};
                <<"proj_key_ult_index">> -> {error, key_already_exists}
            end;
        {error, #error{codename = foreign_key_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"projects_jira_id_fkey">> -> {error, jira_not_exists};
                <<"projects_owner_id_fkey">> -> {error, owner_not_exists}
            end;
        {error, #error{codename = Code}} ->
            {error, Code}
    end.

-spec update(ProjectId, Patch) -> Result when
    ProjectId :: non_neg_integer(),
    Patch :: maps:map(),
    Result :: 'ok' | {'error', Reason :: atom()}.

update(ProjectId, Patch) ->
    update(ProjectId, undefined, Patch).

-spec update(ProjectId, Rev, Patch) -> Result when
    ProjectId :: non_neg_integer(),
    Rev :: non_neg_integer() | 'undefined',
    Patch :: maps:map(),
    Result :: 'ok' | {'error', Reason :: atom()}.

update(ProjectId, Rev, Patch) ->
    % Define all possible fields
    Fields = [
        ?mk_mod_trim(title),
        ?mk_mod_trim(key),
        jira_id,
        ?mk_mod_trim(jira_key),
        ?mk_mod_trim(slack_receivers)
    ],

    % Define auto-set fields
    AutoSetFields = [
        ?mk_mod_inc(rev),
        ?mk_mod_set_now(updated_at)
    ],

    % Perform update
    case db_util:mk_update(<<"projects">>, <<"id">>, ProjectId, <<"rev">>, Rev, Fields, AutoSetFields, Patch) of
        {ok, Query, Params} ->
            case db_query:update(Query, Params, [raw_error]) of
                {ok, 1} -> ok;
                {ok, 0} -> {error, ?err_not_exists};
                {error, #error{codename = check_violation, extra = ExtraProps}} ->
                    case proplists:get_value(constraint_name, ExtraProps, undefined) of
                        <<"projects_title_check">> -> {error, invalid_title};
                        <<"projects_key_check">> -> {error, invalid_key};
                        <<"projects_slack_receivers_check">> -> {error, invalid_slack_receivers}
                    end;
                {error, #error{codename = unique_violation, extra = ExtraProps}} ->
                    case proplists:get_value(constraint_name, ExtraProps, undefined) of
                        <<"projects_title_key">> -> {error, title_already_exists};
                        <<"proj_title_ult_index">> -> {error, title_already_exists};
                        <<"projects_key_key">> -> {error, key_already_exists};
                        <<"proj_key_ult_index">> -> {error, key_already_exists}
                    end;
                {error, #error{codename = foreign_key_violation, extra = ExtraProps}} ->
                    case proplists:get_value(constraint_name, ExtraProps, undefined) of
                        <<"projects_jira_id_fkey">> -> {error, jira_not_exists};
                        <<"projects_owner_id_fkey">> -> {error, owner_not_exists}
                    end;
                {error, #error{codename = Code}} ->
                    {error, Code}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete(ProjectId) -> Result when
    ProjectId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete(ProjectId) ->
    Query = <<"DELETE FROM projects WHERE id = $1">>,
    case db_query:delete(Query, [ProjectId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec exists(ProjectId) -> Result when
    ProjectId :: non_neg_integer(),
    Result :: {'ok', boolean()} | {'error', Reason :: atom()}.

exists(ProjectId) ->
    Query = <<"SELECT COUNT(*)::bigint > 0 AS result FROM projects WHERE id = $1">>,
    case db_query:select_one(Query, [ProjectId]) of
        {ok, #{<<"result">> := Result}} -> {ok, Result};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec is_accessible(ProjectId, PersonnelId) -> Result when
    ProjectId :: non_neg_integer(),
    PersonnelId :: non_neg_integer(),
    Result :: {'ok', boolean()} | {'error', Reason :: atom()}.

is_accessible(ProjectId, PersonnelId) ->
    Query = <<"SELECT is_project_accessible_by_personnel($1, $2) AS result">>,
    case db_query:select_one(Query, [ProjectId, PersonnelId]) of
        {ok, #{<<"result">> := Result}} -> {ok, Result};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % pr  : projects
    % per : personnel
    % ji  : jira_instances
    <<
        "pr.id AS id, ",
        "pr.rev AS rev, ",
        "pr.title AS title, ",
        "pr.key AS key, ",
        "pr.slack_receivers AS slack_receivers, ",
        "pr.owner_id AS owner_id, ",
        "per.username AS owner_name, ",
        "pr.jira_id AS jira_id, ",
        "pr.jira_key AS jira_key, ",
        "ji.title AS jira_title, ",
        "ji.url AS jira_url, ",
        "pr.created_at AS created_at, ",
        "pr.updated_at AS updated_at "
    >>.

common_joins() ->
    <<
        "LEFT OUTER JOIN personnel AS per ON (per.id = pr.owner_id) ",
        "LEFT OUTER JOIN jira_instances AS ji ON (ji.id = pr.jira_id) "
    >>.
