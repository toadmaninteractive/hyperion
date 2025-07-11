-module(db_if_setup_steps).

%% Include files

-include_lib("epgsql/include/epgsql.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/1,
    get_all/1,
    create/6,
    update/2,
    update/3,
    delete/1,
    exists/1,
    parent_id/1,
    project_id/1,
    link_parameter/2,
    unlink_parameter/1,
    linked_parameter_id/1
]).

%% API

-spec get_one(SetupId) -> Result when
    SetupId :: non_neg_integer(),
    Result :: {'ok', protocol_db:setup_step()} | {'error', Reason :: atom()}.

get_one(SetupId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM setup_steps AS sets ",
        (common_joins())/binary,
        "WHERE sets.id = $1"
    >>,
    case db_query:select_one(Query, [SetupId]) of
        {ok, Item} -> {ok, protocol_db:setup_step_from_json(Item)};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_all(ProjectId :: non_neg_integer()) ->
    {'ok', Items :: [protocol_db:setup_step()]} | {'error', Reason :: atom()}.

get_all(ProjectId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM setup_steps AS sets ",
        (common_joins())/binary,
        "WHERE sets.project_id = $1 "
        "ORDER BY order_num ASC, LOWER(title) ASC"
    >>,
    case db_query:select(Query, [ProjectId]) of
        {ok, Items} -> {ok, [protocol_db:setup_step_from_json(Item) || Item <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec create(ParentId, ProjectId, Title, Description, IsDraft, OrderNum) -> Result when
    ParentId :: non_neg_integer() | 'null',
    ProjectId :: non_neg_integer(),
    Title :: binary() | 'null',
    Description :: binary() | 'null',
    IsDraft :: boolean(),
    OrderNum :: integer(),
    Result :: {'ok', SetupId :: non_neg_integer()} | {'error', Reason :: atom()}.

create(ParentId, ProjectId, Title, Description, IsDraft, OrderNum) ->
    Query = <<
        "INSERT INTO setup_steps (parent_id, project_id, title, description, is_draft, order_num) ",
        "VALUES ($1, $2, TRIM($3), TRIM($4), $5, $6) ",
        "RETURNING id::bigint"
    >>,
    Params = [ParentId, ProjectId, Title, Description, IsDraft, OrderNum],
    case db_query:insert(Query, Params, [raw_error]) of
        {ok, 1, Columns, Rows} ->
            [#{<<"id">> := SetupId}] = db_util:result_to_json(Columns, Rows),
            {ok, SetupId};
        {error, #error{codename = unique_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"sets_project_step_title_ult_index">> -> {error, title_already_exists};
                <<"sets_project_step_title_null_parent_index">> -> {error, title_already_exists};
                <<"sets_project_step_title_null_title_index">> -> {error, title_already_exists}
            end;
        {error, #error{codename = foreign_key_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"setup_steps_parent_id_fkey">> -> {error, parent_not_exists};
                <<"setup_steps_project_id_fkey">> -> {error, project_not_exists}
            end;
        {error, #error{codename = Code}} ->
            {error, Code}
    end.

-spec update(SetupId, Patch) -> Result when
    SetupId :: non_neg_integer(),
    Patch :: maps:map(),
    Result :: 'ok' | {'error', Reason :: atom()}.

update(SetupId, Patch) ->
    update(SetupId, undefined, Patch).

-spec update(SetupId, Rev, Patch) -> Result when
    SetupId :: non_neg_integer(),
    Rev :: non_neg_integer() | 'undefined',
    Patch :: maps:map(),
    Result :: 'ok' | {'error', Reason :: atom()}.

update(SetupId, Rev, Patch) ->
    % Define all possible fields
    Fields = [
        parent_id,
        ?mk_mod_trim(title),
        ?mk_mod_trim(description),
        is_draft,
        order_num
    ],

    % Define auto-set fields
    AutoSetFields = [
        ?mk_mod_inc(rev),
        ?mk_mod_set_now(updated_at)
    ],

    % Perform update
    case db_util:mk_update(<<"setup_steps">>, <<"id">>, SetupId, <<"rev">>, Rev, Fields, AutoSetFields, Patch) of
        {ok, Query, Params} ->
            case db_query:update(Query, Params, [raw_error]) of
                {ok, 1} -> ok;
                {ok, 0} -> {error, ?err_not_exists};
                {error, #error{codename = unique_violation, extra = ExtraProps}} ->
                    case proplists:get_value(constraint_name, ExtraProps, undefined) of
                        <<"sets_project_step_title_ult_index">> -> {error, title_already_exists};
                        <<"sets_project_step_title_null_parent_index">> -> {error, title_already_exists};
                        <<"sets_project_step_title_null_title_index">> -> {error, title_already_exists}
                    end;
                {error, #error{codename = foreign_key_violation, extra = ExtraProps}} ->
                    case proplists:get_value(constraint_name, ExtraProps, undefined) of
                        <<"setup_steps_parent_id_fkey">> -> {error, parent_not_exists};
                        <<"setup_steps_project_id_fkey">> -> {error, project_not_exists}
                    end;
                {error, #error{codename = Code}} ->
                    {error, Code}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete(SetupId) -> Result when
    SetupId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete(SetupId) ->
    Query = <<"DELETE FROM setup_steps WHERE id = $1">>,
    case db_query:delete(Query, [SetupId], [raw_error]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, #error{codename = foreign_key_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"setup_steps_parent_id_fkey">> -> {error, has_children}
            end;
        {error, #error{codename = Code}} ->
            {error, Code}
    end.

-spec exists(SetupId) -> Result when
    SetupId :: non_neg_integer(),
    Result :: {'ok', boolean()} | {'error', Reason :: atom()}.

exists(SetupId) ->
    Query = <<"SELECT COUNT(*)::bigint > 0 AS result FROM setup_steps WHERE id = $1">>,
    case db_query:select_one(Query, [SetupId]) of
        {ok, #{<<"result">> := Result}} -> {ok, Result};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec parent_id(SetupId) -> Result when
    SetupId :: non_neg_integer(),
    Result :: {'ok', non_neg_integer() | 'undefined'} | {'error', Reason :: atom()}.

parent_id(SetupId) ->
    Query = <<"SELECT parent_id FROM setup_steps WHERE id = $1">>,
    case db_query:select_one(Query, [SetupId]) of
        {ok, #{<<"parent_id">> := ?null}} -> {ok, undefined};
        {ok, #{<<"parent_id">> := ParentId}} -> {ok, ParentId};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec project_id(SetupId) -> Result when
    SetupId :: non_neg_integer(),
    Result :: {'ok', non_neg_integer()} | {'error', Reason :: atom()}.

project_id(SetupId) ->
    Query = <<"SELECT project_id FROM setup_steps WHERE id = $1">>,
    case db_query:select_one(Query, [SetupId]) of
        {ok, #{<<"project_id">> := ProjectId}} -> {ok, ProjectId};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec link_parameter(SetupId, ParamId) -> Result when
    SetupId :: non_neg_integer(),
    ParamId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

link_parameter(SetupId, ParamId) ->
    Query = <<"SELECT link_setup_to_project_parameter($1, $2) AS result">>,
    Params = [SetupId, ParamId],
    case db_query:transaction({Query, Params}) of
        {ok, #{result := [#{<<"result">> := <<"ok">>}|_]}} -> ok;
        {ok, #{result := [#{<<"result">> := <<"setup_step_not_exists">>}|_]}} -> {error, setup_step_not_exists};
        {ok, #{result := [#{<<"result">> := <<"parameter_not_exists">>}|_]}} -> {error, parameter_not_exists};
        {ok, #{result := [#{<<"result">> := <<"project_mismatch">>}|_]}} -> {error, project_mismatch};
        {error, Reason} -> {error, Reason}
    end.

-spec unlink_parameter(SetupId) -> Result when
    SetupId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

unlink_parameter(SetupId) ->
    Query = <<"DELETE FROM parameter_setup_links WHERE setup_id = $1">>,
    case db_query:delete(Query, [SetupId], [raw_error]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, #error{codename = Code}} -> {error, Code}
    end.

-spec linked_parameter_id(SetupId) -> Result when
    SetupId :: non_neg_integer(),
    Result :: {'ok', non_neg_integer()} | {'error', Reason :: atom()}.

linked_parameter_id(SetupId) ->
    Query = <<"SELECT parameter_id FROM parameter_setup_links WHERE setup_id = $1">>,
    case db_query:select_one(Query, [SetupId]) of
        {ok, #{<<"parameter_id">> := ParameterId}} -> {ok, ParameterId};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % sets : setup_steps
    % psl  : parameter_setup_links
    <<
        "sets.id AS id, ",
        "sets.rev AS rev, ",
        "sets.parent_id AS parent_id, ",
        "sets.project_id AS project_id, ",
        "sets.title AS title, ",
        "sets.description AS description, ",
        "sets.is_draft AS is_draft, ",
        "sets.order_num AS order_num, ",
        "psl.parameter_id AS parameter_id, ",
        "sets.created_at AS created_at, ",
        "sets.updated_at AS updated_at "
    >>.

common_joins() ->
    <<
        "LEFT OUTER JOIN parameter_setup_links AS psl ON (psl.setup_id = sets.id) "
    >>.
