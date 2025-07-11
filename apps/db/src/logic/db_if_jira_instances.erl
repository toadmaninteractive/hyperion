-module(db_if_jira_instances).

%% Include files

-include_lib("epgsql/include/epgsql.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/1,
    get_all/0,
    create/2,
    update/2,
    update/3,
    delete/1,
    exists/1
]).

%% API

-spec get_one(JiraId) -> Result when
    JiraId :: non_neg_integer(),
    Result :: {'ok', protocol_db:jira_instance()} | {'error', Reason :: atom()}.

get_one(JiraId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM jira_instances AS ji ",
        (common_joins())/binary,
        "WHERE ji.id = $1"
    >>,
    case db_query:select_one(Query, [JiraId]) of
        {ok, Item} -> {ok, protocol_db:jira_instance_from_json(Item)};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_all() ->
    {'ok', Items :: [protocol_db:jira_instance()]} | {'error', Reason :: atom()}.

get_all() ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM jira_instances AS ji ",
        (common_joins())/binary,
        "ORDER BY LOWER(title) ASC"
    >>,
    case db_query:select(Query, []) of
        {ok, Items} -> {ok, [protocol_db:jira_instance_from_json(Item) || Item <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec create(Title, Url) -> Result when
    Title :: binary(),
    Url :: binary(),
    Result :: {'ok', JiraId :: non_neg_integer()} | {'error', Reason :: atom()}.

create(Title, Url) ->
    Query = <<
        "INSERT INTO jira_instances (title, url) ",
        "VALUES (TRIM($1), TRIM($2)) ",
        "RETURNING id::bigint"
    >>,
    Params = [Title, Url],
    case db_query:insert(Query, Params, [raw_error]) of
        {ok, 1, Columns, Rows} ->
            [#{<<"id">> := JiraId}] = db_util:result_to_json(Columns, Rows),
            {ok, JiraId};
        {error, #error{codename = check_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"jira_instances_title_check">> -> {error, invalid_title};
                <<"jira_instances_url_check">> -> {error, invalid_url}
            end;
        {error, #error{codename = unique_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"jira_instances_title_key">> -> {error, title_already_exists};
                <<"ji_title_ult_index">> -> {error, title_already_exists}
            end;
        {error, #error{codename = Code}} ->
            {error, Code}
    end.

-spec update(JiraId, Patch) -> Result when
    JiraId :: non_neg_integer(),
    Patch :: maps:map(),
    Result :: 'ok' | {'error', Reason :: atom()}.

update(JiraId, Patch) ->
    update(JiraId, undefined, Patch).

-spec update(JiraId, Rev, Patch) -> Result when
    JiraId :: non_neg_integer(),
    Rev :: non_neg_integer() | 'undefined',
    Patch :: maps:map(),
    Result :: 'ok' | {'error', Reason :: atom()}.

update(JiraId, Rev, Patch) ->
    % Define all possible fields
    Fields = [
        ?mk_mod_trim(title),
        ?mk_mod_trim(url)
    ],

    % Define auto-set fields
    AutoSetFields = [
        ?mk_mod_inc(rev),
        ?mk_mod_set_now(updated_at)
    ],

    % Perform update
    case db_util:mk_update(<<"jira_instances">>, <<"id">>, JiraId, <<"rev">>, Rev, Fields, AutoSetFields, Patch) of
        {ok, Query, Params} ->
            case db_query:update(Query, Params, [raw_error]) of
                {ok, 1} -> ok;
                {ok, 0} -> {error, ?err_not_exists};
                {error, #error{codename = check_violation, extra = ExtraProps}} ->
                    case proplists:get_value(constraint_name, ExtraProps, undefined) of
                        <<"jira_instances_title_check">> -> {error, invalid_title};
                        <<"jira_instances_url_check">> -> {error, invalid_url}
                    end;
                {error, #error{codename = unique_violation, extra = ExtraProps}} ->
                    case proplists:get_value(constraint_name, ExtraProps, undefined) of
                        <<"jira_instances_title_key">> -> {error, title_already_exists};
                        <<"ji_title_ult_index">> -> {error, title_already_exists}
                    end;
                {error, #error{codename = Code}} ->
                    {error, Code}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete(JiraId) -> Result when
    JiraId :: non_neg_integer(),
    Result :: {'ok', boolean()} | {'error', Reason :: atom()}.

delete(JiraId) ->
    Query = <<"SELECT delete_jira_instance($1) AS result">>,
    case db_query:transaction({Query, [JiraId]}) of
        {ok, #{result := [#{<<"result">> := Result}|_]}} -> {ok, Result};
        {error, Reason} -> {error, Reason}
    end.

-spec exists(JiraId) -> Result when
    JiraId :: non_neg_integer(),
    Result :: {'ok', boolean()} | {'error', Reason :: atom()}.

exists(JiraId) ->
    Query = <<"SELECT COUNT(*)::bigint > 0 AS result FROM jira_instances WHERE id = $1">>,
    case db_query:select_one(Query, [JiraId]) of
        {ok, #{<<"result">> := Result}} -> {ok, Result};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % ji : jira_instances
    <<
        "ji.id AS id, ",
        "ji.rev AS rev, ",
        "ji.title AS title, ",
        "ji.url AS url, ",
        "ji.created_at AS created_at, ",
        "ji.updated_at AS updated_at "
    >>.

common_joins() ->
    <<>>.
