-module(db_if_test_run_items).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/1,
    get_all/1,
    get_for_setup/3,
    create/5,
    update/2,
    update/3,
    delete/1,
    delete_all/1,
    exists/1,
    status/1,
    run_id/1,
    project_id/1,
    jira_issue_key/1
]).

%% API

-spec get_one(ItemId) -> Result when
    ItemId :: non_neg_integer(),
    Result :: {'ok', protocol_db:test_run_item()} | {'error', Reason :: atom()}.

get_one(ItemId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM test_run_items AS tri ",
        (common_joins())/binary,
        "WHERE tri.id = $1"
    >>,
    case db_query:select_one(Query, [ItemId]) of
        {ok, Item} -> {ok, protocol_db:test_run_item_from_json(Item)};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_all(RunId) -> Result when
    RunId :: non_neg_integer(),
    Result :: {'ok', Items :: [protocol_db:test_run_item()]} | {'error', Reason :: atom()}.

get_all(RunId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM test_run_items AS tri ",
        (common_joins())/binary,
        "WHERE tri.run_id = $1 "
        "ORDER BY order_num ASC, id ASC"
    >>,
    case db_query:select(Query, [RunId]) of
        {ok, Items} -> {ok, [protocol_db:test_run_item_from_json(Item) || Item <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec get_for_setup(RunId, SetupId, ActiveOnly) -> Result when
    RunId :: non_neg_integer(),
    SetupId :: non_neg_integer(),
    ActiveOnly :: boolean(),
    Result :: {'ok', Items :: [protocol_db:test_run_item()]} | {'error', Reason :: atom()}.

get_for_setup(RunId, SetupId, ActiveOnly) ->
    SubQuery = ?yesno(ActiveOnly, <<" AND tri.status IN ('pending', 'in_progress') ">>, <<>>),
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM test_run_items AS tri ",
        (common_joins())/binary,
        "WHERE tri.run_id = $1 AND tri.case_id IN (",
            "SELECT id FROM test_cases WHERE setup_id IN (",
                "SELECT t.id FROM (",
                    "WITH RECURSIVE search_graph(id, parent_id, title, depth, path, cycle) AS (",
                        "SELECT g.id, g.parent_id, g.title, 1, ARRAY[g.id], false FROM setup_steps g ",
                        "UNION ALL ",
                        "SELECT g.id, g.parent_id, g.title, sg.depth + 1, path || g.id, g.id = ANY(path) ",
                        "FROM setup_steps g, search_graph sg ",
                        "WHERE g.parent_id = sg.id AND NOT cycle ",
                    ") ",
                    "SELECT DISTINCT(id) FROM search_graph WHERE array_position(path::bigint[], $2::bigint) IS NOT NULL ",
                ") AS t ",
            ") ",
        ") ",
        SubQuery/binary,
        "ORDER BY id ASC"
    >>,
    case db_query:select(Query, [RunId, SetupId]) of
        {ok, Items} -> {ok, [protocol_db:test_run_item_from_json(Item) || Item <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec create(RunId, CaseId, ReporterId, OrderNum, Parameters) -> Result when
    RunId :: non_neg_integer(),
    CaseId :: non_neg_integer(),
    ReporterId :: non_neg_integer(),
    OrderNum :: integer(),
    Parameters :: protocol_db:test_run_item_params(),
    Result :: {'ok', ItemId :: non_neg_integer()} | {'error', Reason :: atom()}.

create(RunId, CaseId, ReporterId, OrderNum, Parameters) ->
    Query = <<
        "INSERT INTO test_run_items (run_id, case_id, reporter_id, order_num, params) ",
        "VALUES ($1, $2, $3, $4, $5) ",
        "RETURNING id::bigint"
    >>,
    ParamsJson = jsx:encode(protocol_db:test_run_item_params_to_json(Parameters)),
    Params = [RunId, CaseId, ReporterId, OrderNum, ParamsJson],
    case db_query:insert(Query, Params, [raw_error]) of
        {ok, 1, Columns, Rows} ->
            [#{<<"id">> := ItemId}] = db_util:result_to_json(Columns, Rows),
            {ok, ItemId};
        {error, #error{codename = foreign_key_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"test_run_items_run_id_fkey">> -> {error, test_run_not_exists};
                <<"test_run_items_case_id_fkey">> -> {error, test_case_not_exists};
                <<"test_run_items_reporter_id_fkey">> -> {error, reporter_not_exists}
            end;
        {error, #error{codename = Code}} ->
            {error, Code}
    end.

-spec update(ItemId, Patch) -> Result when
    ItemId :: non_neg_integer(),
    Patch :: maps:map(),
    Result :: 'ok' | {'error', Reason :: atom()}.

update(ItemId, Patch) ->
    update(ItemId, undefined, Patch).

-spec update(ItemId, Rev, Patch) -> Result when
    ItemId :: non_neg_integer(),
    Rev :: non_neg_integer() | 'undefined',
    Patch :: maps:map(),
    Result :: 'ok' | {'error', Reason :: atom()}.

update(ItemId, Rev, Patch) ->
    % Define all possible fields
    Fields = [
        assignee_id,
        status,
        ?mk_mod_trim(summary),
        failed_setup_id,
        order_num,
        ?mk_mod_trim(jira_issue_key),
        ?mk_mod_coalesce(started_at),
        finished_at
    ],

    % Define auto-set fields
    AutoSetFields = [
        ?mk_mod_inc(rev),
        ?mk_mod_set_now(updated_at)
    ],

    % Perform update
    case db_util:mk_update(<<"test_run_items">>, <<"id">>, ItemId, <<"rev">>, Rev, Fields, AutoSetFields, Patch) of
        {ok, Query, Params} ->
            case db_query:update(Query, Params, [raw_error]) of
                {ok, 1} -> ok;
                {ok, 0} -> {error, ?err_not_exists};
                {error, #error{codename = foreign_key_violation, extra = ExtraProps}} ->
                    case proplists:get_value(constraint_name, ExtraProps, undefined) of
                        <<"test_run_items_assignee_id_fkey">> -> {error, assignee_not_exists};
                        <<"test_run_items_failed_setup_id_fkey">> -> {error, setup_not_exists}
                    end;
                {error, #error{codename = Code}} ->
                    {error, Code}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete(ItemId) -> Result when
    ItemId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete(ItemId) ->
    Query = <<"DELETE FROM test_run_items WHERE id = $1">>,
    case db_query:delete(Query, [ItemId], [raw_error]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, #error{codename = Code}} -> {error, Code}
    end.

-spec delete_all(RunId) -> Result when
    RunId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete_all(RunId) ->
    Query = <<"DELETE FROM test_run_items WHERE run_id = $1">>,
    case db_query:delete(Query, [RunId], [raw_error]) of
        {ok, _} -> ok;
        {error, #error{codename = Code}} -> {error, Code}
    end.

-spec exists(ItemId) -> Result when
    ItemId :: non_neg_integer(),
    Result :: {'ok', boolean()} | {'error', Reason :: atom()}.

exists(ItemId) ->
    Query = <<"SELECT COUNT(*)::bigint > 0 AS result FROM test_run_items WHERE id = $1">>,
    case db_query:select_one(Query, [ItemId]) of
        {ok, #{<<"result">> := Result}} -> {ok, Result};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec status(ItemId) -> Result when
    ItemId :: non_neg_integer(),
    Result :: {'ok', protocol_db:test_status()} | {'error', Reason :: atom()}.

status(ItemId) ->
    Query = <<"SELECT status FROM test_run_items WHERE id = $1">>,
    case db_query:select_one(Query, [ItemId]) of
        {ok, #{<<"status">> := Status}} -> {ok, protocol_db:test_status_from_json(Status)};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec run_id(ItemId) -> Result when
    ItemId :: non_neg_integer(),
    Result :: {'ok', non_neg_integer()} | {'error', Reason :: atom()}.

run_id(ItemId) ->
    Query = <<"SELECT run_id FROM test_run_items WHERE id = $1">>,
    case db_query:select_one(Query, [ItemId]) of
        {ok, #{<<"run_id">> := RunId}} -> {ok, RunId};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec project_id(ItemId) -> Result when
    ItemId :: non_neg_integer(),
    Result :: {'ok', non_neg_integer()} | {'error', Reason :: atom()}.

project_id(ItemId) ->
    Query = <<
        "SELECT tr.project_id AS project_id ",
        "FROM test_run_items AS tri ",
        "LEFT OUTER JOIN test_runs AS tr ON (tr.id = tri.run_id) ",
        "WHERE tri.id = $1"
    >>,
    case db_query:select_one(Query, [ItemId]) of
        {ok, #{<<"project_id">> := ProjectId}} -> {ok, ProjectId};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec jira_issue_key(ItemId) -> Result when
    ItemId :: non_neg_integer(),
    Result :: {'ok', binary() | 'undefined'} | {'error', Reason :: atom()}.

jira_issue_key(ItemId) ->
    Query = <<"SELECT jira_issue_key FROM test_run_items WHERE id = $1">>,
    case db_query:select_one(Query, [ItemId]) of
        {ok, #{<<"jira_issue_key">> := JiraIssueKey}} -> {ok, ?yesno(is_binary(JiraIssueKey), JiraIssueKey, undefined)};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % tri : test_run_items
    % tr  : test_runs
    % pr  : projects
    % ji  : jira_instances
    <<
        "tri.id AS id, ",
        "tri.rev AS rev, ",
        "tri.run_id AS run_id, ",
        "tri.case_id AS case_id, ",
        "tri.reporter_id AS reporter_id, ",
        "per1.username AS reporter_name, ",
        "tri.assignee_id AS assignee_id, ",
        "per2.username AS assignee_name, ",
        "tri.status AS status, ",
        "tri.summary AS summary, ",
        "tri.failed_setup_id AS failed_setup_id, ",
        "tri.order_num AS order_num, ",
        "tri.params AS params, ",
        "tri.jira_issue_key AS jira_issue_key, ",
        "(CASE ",
            "WHEN (tri.jira_issue_key IS NOT NULL AND ji.url IS NOT NULL) THEN CONCAT(ji.url, '/browse/', tri.jira_issue_key) ",
            "ELSE NULL ",
        "END) AS jira_issue_url,"
        "tri.created_at AS created_at, ",
        "tri.updated_at AS updated_at, ",
        "tri.started_at AS started_at, ",
        "tri.finished_at AS finished_at "
    >>.

common_joins() ->
    <<
        "LEFT OUTER JOIN personnel AS per1 ON (per1.id = tri.reporter_id) ",
        "LEFT OUTER JOIN personnel AS per2 ON (per2.id = tri.assignee_id) ",
        "LEFT OUTER JOIN test_runs AS tr ON (tr.id = tri.run_id) ",
        "LEFT OUTER JOIN projects AS pr ON (pr.id = tr.project_id) ",
        "LEFT OUTER JOIN jira_instances AS ji ON (ji.id = pr.jira_id) "
    >>.
