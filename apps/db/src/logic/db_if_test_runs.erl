-module(db_if_test_runs).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/1,
    get_all/1,
    get/6,
    get_count/2,
    create/2,
    update/2,
    update/3,
    delete/1,
    exists/1,
    status/1,
    project_id/1
]).

%% API

-spec get_one(RunId) -> Result when
    RunId :: non_neg_integer(),
    Result :: {'ok', protocol_db:test_run()} | {'error', Reason :: atom()}.

get_one(RunId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM test_runs AS tr ",
        (common_joins())/binary,
        "WHERE tr.id = $1"
    >>,
    case db_query:select_one(Query, [RunId]) of
        {ok, Item} -> {ok, protocol_db:test_run_from_json(Item)};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_all(ProjectId) -> Result when
    ProjectId :: non_neg_integer(),
    Result :: {'ok', Items :: [protocol_db:test_run()]} | {'error', Reason :: atom()}.

get_all(ProjectId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM test_runs AS tr ",
        (common_joins())/binary,
        "WHERE tr.project_id = $1 "
        "ORDER BY id ASC"
    >>,
    case db_query:select(Query, [ProjectId]) of
        {ok, Items} -> {ok, [protocol_db:test_run_from_json(Item) || Item <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec get(ProjectId, OrderBy, OrderDir, Offset, Limit, Status) -> Result when
    ProjectId :: non_neg_integer(),
    OrderBy :: binary(),
    OrderDir :: binary(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Status :: binary() | 'null',
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get(ProjectId, OrderBy, OrderDir, Offset, Limit, Status) ->
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    SubQuery = ?yesno(is_binary(Status), <<" AND tr.status = $2 ">>, <<>>),
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM test_runs AS tr ",
        (common_joins())/binary,
        "WHERE tr.project_id = $1 ",
        SubQuery/binary,
        Filter/binary
    >>,
    Params = [ProjectId] ++ ?yesno(is_binary(Status), [Status], []),
    case db_query:select(Query, Params) of
        {ok, Items} -> {ok, [protocol_db:test_run_from_json(Item) || Item <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec get_count(ProjectId, Status) -> Result when
    ProjectId :: non_neg_integer(),
    Status :: binary() | 'null',
    Result :: {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_count(ProjectId, Status) ->
    SubQuery = ?yesno(is_binary(Status), <<" AND status = $2 ">>, <<>>),
    Query = <<
        "SELECT COUNT(*)::bigint AS count ",
        "FROM test_runs ",
        "WHERE project_id = $1 ",
        SubQuery/binary
    >>,
    Params = [ProjectId] ++ ?yesno(is_binary(Status), [Status], []),
    case db_query:select_one(Query, Params) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec create(ProjectId, Title) -> Result when
    ProjectId :: non_neg_integer(),
    Title :: binary() | 'undefined',
    Result :: {'ok', RunId :: non_neg_integer()} | {'error', Reason :: atom()}.

create(ProjectId, Title) ->
    Query = <<
        "INSERT INTO test_runs (project_id, title) ",
        "VALUES ($1, $2) ",
        "RETURNING id::bigint"
    >>,
    Params = [ProjectId, Title],
    case db_query:insert(Query, Params, [raw_error]) of
        {ok, 1, Columns, Rows} ->
            [#{<<"id">> := RunId}] = db_util:result_to_json(Columns, Rows),
            {ok, RunId};
        {error, #error{codename = unique_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"tr_project_title_index">> -> {error, title_already_exists}
            end;
        {error, #error{codename = foreign_key_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"test_runs_project_id_fkey">> -> {error, project_not_exists}
            end;
        {error, #error{codename = Code}} ->
            {error, Code}
    end.

-spec update(RunId, Patch) -> Result when
    RunId :: non_neg_integer(),
    Patch :: maps:map(),
    Result :: 'ok' | {'error', Reason :: atom()}.

update(RunId, Patch) ->
    update(RunId, undefined, Patch).

-spec update(RunId, Rev, Patch) -> Result when
    RunId :: non_neg_integer(),
    Rev :: non_neg_integer() | 'undefined',
    Patch :: maps:map(),
    Result :: 'ok' | {'error', Reason :: atom()}.

update(RunId, Rev, Patch) ->
    % Define all possible fields
    Fields = [
        ?mk_mod_trim(title),
        status,
        ?mk_mod_coalesce(started_at),
        finished_at
    ],

    % Define auto-set fields
    AutoSetFields = [
        ?mk_mod_inc(rev),
        ?mk_mod_set_now(updated_at)
    ],

    % Perform update
    case db_util:mk_update(<<"test_runs">>, <<"id">>, RunId, <<"rev">>, Rev, Fields, AutoSetFields, Patch) of
        {ok, Query, Params} ->
            case db_query:update(Query, Params, [raw_error]) of
                {ok, 1} -> ok;
                {ok, 0} -> {error, ?err_not_exists};
                {error, #error{codename = unique_violation, extra = ExtraProps}} ->
                    case proplists:get_value(constraint_name, ExtraProps, undefined) of
                        <<"tr_project_title_index">> -> {error, title_already_exists}
                    end;
                {error, #error{codename = foreign_key_violation, extra = ExtraProps}} ->
                    case proplists:get_value(constraint_name, ExtraProps, undefined) of
                        <<"test_runs_project_id_fkey">> -> {error, project_not_exists}
                    end;
                {error, #error{codename = Code}} ->
                    {error, Code}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete(RunId) -> Result when
    RunId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete(RunId) ->
    Query = <<"DELETE FROM test_runs WHERE id = $1">>,
    case db_query:delete(Query, [RunId], [raw_error]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, #error{codename = Code}} -> {error, Code}
    end.

-spec exists(RunId) -> Result when
    RunId :: non_neg_integer(),
    Result :: {'ok', boolean()} | {'error', Reason :: atom()}.

exists(RunId) ->
    Query = <<"SELECT COUNT(*)::bigint > 0 AS result FROM test_runs WHERE id = $1">>,
    case db_query:select_one(Query, [RunId]) of
        {ok, #{<<"result">> := Result}} -> {ok, Result};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec status(RunId) -> Result when
    RunId :: non_neg_integer(),
    Result :: {'ok', protocol_db:test_run_status()} | {'error', Reason :: atom()}.

status(RunId) ->
    Query = <<"SELECT status FROM test_runs WHERE id = $1">>,
    case db_query:select_one(Query, [RunId]) of
        {ok, #{<<"status">> := Status}} -> {ok, protocol_db:test_run_status_from_json(Status)};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec project_id(RunId) -> Result when
    RunId :: non_neg_integer(),
    Result :: {'ok', non_neg_integer()} | {'error', Reason :: atom()}.

project_id(RunId) ->
    Query = <<"SELECT project_id FROM test_runs WHERE id = $1">>,
    case db_query:select_one(Query, [RunId]) of
        {ok, #{<<"project_id">> := ProjectId}} -> {ok, ProjectId};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % tr : test_runs
    <<
        "tr.id AS id, ",
        "tr.rev AS rev, ",
        "tr.project_id AS project_id, ",
        "tr.title AS title, ",
        "tr.status AS status, ",
        "tr.created_at AS created_at, ",
        "tr.updated_at AS updated_at, ",
        "tr.started_at AS started_at, ",
        "tr.finished_at AS finished_at, ",
        "(SELECT COUNT(*)::integer FROM test_run_items AS tri1 WHERE tri1.run_id = tr.id) AS total_item_count, ",
        "(SELECT COUNT(*)::integer FROM test_run_items AS tri2 WHERE tri2.run_id = tr.id AND tri2.status = 'pending') AS pending_item_count, ",
        "(SELECT COUNT(*)::integer FROM test_run_items AS tri3 WHERE tri3.run_id = tr.id AND tri3.status = 'in_progress') AS in_progress_item_count, ",
        "(SELECT COUNT(*)::integer FROM test_run_items AS tri4 WHERE tri4.run_id = tr.id AND tri4.status = 'passed') AS passed_item_count, ",
        "(SELECT COUNT(*)::integer FROM test_run_items AS tri5 WHERE tri5.run_id = tr.id AND tri5.status = 'failed') AS failed_item_count, ",
        "(SELECT COUNT(*)::integer FROM test_run_items AS tri6 WHERE tri6.run_id = tr.id AND tri6.status = 'blocked') AS blocked_item_count "
    >>.

common_joins() ->
    <<>>.
