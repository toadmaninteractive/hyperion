-module(db_if_parameters).

%% Include files

-include_lib("epgsql/include/epgsql.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/1,
    get_for_project/1,
    create/4,
    rename/2,
    delete/1,
    add_value/3,
    rename_value/3,
    remove_value/3,
    value_id/2,
    project_id/1
]).

%% API

-spec get_one(ParamId) -> Result when
    ParamId :: non_neg_integer(),
    Result :: {'ok', protocol_db:parameter()} | {'error', Reason :: atom()}.

get_one(ParamId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM parameters AS parm ",
        (common_joins())/binary,
        "WHERE parm.id = $1 ",
        "GROUP BY parm.id "
    >>,
    case db_query:select_one(Query, [ParamId]) of
        {ok, Item} -> {ok, protocol_db:parameter_from_json(Item)};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_for_project(ProjectId) -> Result when
    ProjectId :: non_neg_integer(),
    Result :: {'ok', Items :: [protocol_db:parameter()]} | {'error', Reason :: atom()}.

get_for_project(ProjectId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM parameters AS parm ",
        (common_joins())/binary,
        "WHERE parm.project_id = $1 ",
        "GROUP BY parm.id ",
        "ORDER BY LOWER(title) ASC"
    >>,
    case db_query:select(Query, [ProjectId]) of
        {ok, Items} -> {ok, [protocol_db:parameter_from_json(Item) || Item <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec create(ProjectId, ParentId, DependentId, Title) -> Result when
    ProjectId :: non_neg_integer(),
    ParentId :: non_neg_integer() | 'undefined',
    DependentId :: non_neg_integer() | 'undefined',
    Title :: binary(),
    Result :: {'ok', ParamId :: non_neg_integer()} | {'error', Reason :: atom()}.

create(ProjectId, ParentId, DependentId, Title) ->
    Query = <<"SELECT create_project_parameter($1, $2, $3, TRIM($4)) AS result">>,
    Params = [ProjectId, ParentId, DependentId, Title],
    case db_query:transaction({Query, Params}) of
        {ok, #{result := [#{<<"result">> := <<"project_not_exists">>}|_]}} -> {error, project_not_exists};
        {ok, #{result := [#{<<"result">> := <<"parameter_already_exists">>}|_]}} -> {error, parameter_already_exists};
        {ok, #{result := [#{<<"result">> := <<"parent_not_exists">>}|_]}} -> {error, parent_not_exists};
        {ok, #{result := [#{<<"result">> := <<"parent_is_independent">>}|_]}} -> {error, parent_is_independent};
        {ok, #{result := [#{<<"result">> := <<"parameter_source_already_exists">>}|_]}} -> {error, parameter_source_already_exists};
        {ok, #{result := [#{<<"result">> := ParamId}|_]}} -> {ok, binary_to_integer(ParamId)};
        {error, Reason} -> {error, Reason}
    end.

-spec rename(ParamId, NewTitle) -> Result when
    ParamId :: non_neg_integer(),
    NewTitle :: binary(),
    Result :: 'ok' | {'error', Reason :: atom()}.

rename(ParamId, NewTitle) ->
    Query = <<"SELECT rename_project_parameter($1, TRIM($2)) AS result">>,
    Params = [ParamId, NewTitle],
    case db_query:transaction({Query, Params}) of
        {ok, #{result := [#{<<"result">> := <<"ok">>}|_]}} -> ok;
        {ok, #{result := [#{<<"result">> := <<"parameter_not_exists">>}|_]}} -> {error, parameter_not_exists};
        {ok, #{result := [#{<<"result">> := <<"title_already_exists">>}|_]}} -> {error, title_already_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec delete(ParamId) -> Result when
    ParamId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete(ParamId) ->
    Query = <<"SELECT remove_project_parameter($1) AS result">>,
    case db_query:transaction({Query, [ParamId]}) of
        {ok, #{result := [#{<<"result">> := <<"ok">>}|_]}} -> ok;
        {ok, #{result := [#{<<"result">> := <<"parameter_not_exists">>}|_]}} -> {error, parameter_not_exists};
        {ok, #{result := [#{<<"result">> := <<"has_children">>}|_]}} -> {error, has_children};
        {ok, #{result := [#{<<"result">> := <<"has_dependants">>}|_]}} -> {error, has_dependants};
        {error, Reason} -> {error, Reason}
    end.

-spec add_value(ParamId, DependentValue, Value) -> Result when
    ParamId :: non_neg_integer(),
    DependentValue :: binary() | 'undefined',
    Value :: binary(),
    Result :: 'ok' | {'error', Reason :: atom()}.

add_value(ParamId, DependentValue, Value) ->
    Query = <<"SELECT add_project_parameter_value($1, TRIM($2), TRIM($3)) AS result">>,
    Params = [ParamId, DependentValue, Value],
    case db_query:transaction({Query, Params}) of
        {ok, #{result := [#{<<"result">> := <<"ok">>}|_]}} -> ok;
        {ok, #{result := [#{<<"result">> := <<"parameter_not_exists">>}|_]}} -> {error, parameter_not_exists};
        {ok, #{result := [#{<<"result">> := <<"parameter_source_not_exists">>}|_]}} -> {error, parameter_source_not_exists};
        {ok, #{result := [#{<<"result">> := <<"value_already_exists">>}|_]}} -> {error, value_already_exists};
        {ok, #{result := [#{<<"result">> := <<"value_not_exists">>}|_]}} -> {error, value_not_exists};
        {ok, #{result := [#{<<"result">> := <<"dependent_parameter_not_exists">>}|_]}} -> {error, dependent_parameter_not_exists};
        {ok, #{result := [#{<<"result">> := <<"dependent_source_not_exists">>}|_]}} -> {error, dependent_source_not_exists};
        {ok, #{result := [#{<<"result">> := <<"dependent_value_not_exists">>}|_]}} -> {error, dependent_value_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec rename_value(ParamId, OldValue, NewValue) -> Result when
    ParamId :: non_neg_integer(),
    OldValue :: binary(),
    NewValue :: binary(),
    Result :: 'ok' | {'error', Reason :: atom()}.

rename_value(ParamId, OldValue, NewValue) ->
    Query = <<"SELECT rename_project_parameter_value($1, TRIM($2), TRIM($3)) AS result">>,
    Params = [ParamId, OldValue, NewValue],
    case db_query:transaction({Query, Params}) of
        {ok, #{result := [#{<<"result">> := <<"ok">>}|_]}} -> ok;
        {ok, #{result := [#{<<"result">> := <<"parameter_not_exists">>}|_]}} -> {error, parameter_not_exists};
        {ok, #{result := [#{<<"result">> := <<"invalid_parameter">>}|_]}} -> {error, invalid_parameter};
        {ok, #{result := [#{<<"result">> := <<"value_not_exists">>}|_]}} -> {error, value_not_exists};
        {ok, #{result := [#{<<"result">> := <<"value_already_exists">>}|_]}} -> {error, value_already_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec remove_value(ParamId, DependentValue, Value) -> Result when
    ParamId :: non_neg_integer(),
    DependentValue :: binary() | 'undefined',
    Value :: binary(),
    Result :: 'ok' | {'error', Reason :: atom()}.

remove_value(ParamId, DependentValue, Value) ->
    Query = <<"SELECT remove_project_parameter_value($1, TRIM($2), TRIM($3)) AS result">>,
    Params = [ParamId, DependentValue, Value],
    case db_query:transaction({Query, Params}) of
        {ok, #{result := [#{<<"result">> := <<"ok">>}|_]}} -> ok;
        {ok, #{result := [#{<<"result">> := <<"parameter_not_exists">>}|_]}} -> {error, parameter_not_exists};
        {ok, #{result := [#{<<"result">> := <<"value_not_exists">>}|_]}} -> {error, value_not_exists};
        {ok, #{result := [#{<<"result">> := <<"linked_to_children">>}|_]}} -> {error, linked_to_children};
        {ok, #{result := [#{<<"result">> := <<"linked_to_dependants">>}|_]}} -> {error, linked_to_dependants};
        {ok, #{result := [#{<<"result">> := <<"dependent_parameter_not_exists">>}|_]}} -> {error, dependent_parameter_not_exists};
        {ok, #{result := [#{<<"result">> := <<"dependent_source_not_exists">>}|_]}} -> {error, dependent_source_not_exists};
        {ok, #{result := [#{<<"result">> := <<"dependent_value_not_exists">>}|_]}} -> {error, dependent_value_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec value_id(ParamId, Value) -> Result when
    ParamId :: non_neg_integer(),
    Value :: binary(),
    Result :: {'ok', non_neg_integer()} | {'error', Reason :: atom()}.

value_id(ParamId, Value) ->
    Query = <<
        "SELECT pvl.value_id AS value_id ",
        "FROM parameter_value_links AS pvl ",
        "LEFT OUTER JOIN parameter_source_values AS psv ON (psv.id = pvl.value_id) ",
        "WHERE pvl.parameter_id = $1 AND TRIM(LOWER(psv.value)) = TRIM(LOWER($2)) "
    >>,
    Params = [ParamId, Value],
    case db_query:select_one(Query, Params) of
        {ok, #{<<"value_id">> := ValueId}} -> {ok, ValueId};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec project_id(ParamId) -> Result when
    ParamId :: non_neg_integer(),
    Result :: {'ok', non_neg_integer()} | {'error', Reason :: atom()}.

project_id(ParamId) ->
    Query = <<"SELECT project_id FROM parameters WHERE id = $1">>,
    case db_query:select_one(Query, [ParamId]) of
        {ok, #{<<"project_id">> := ProjectId}} -> {ok, ProjectId};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % parm : parameters
    % pvl  : parameter_value_links
    % psv  : parameter_source_values
    % psvd : parameter_source_values
    <<
        "parm.id AS id, ",
        "parm.rev AS rev, ",
        "parm.project_id AS project_id, ",
        "parm.parent_id AS parent_id, ",
        "parm.dependent_id AS dependent_id, ",
        "parm.title AS title, ",
        "COALESCE(",
            "jsonb_agg(jsonb_build_object('value', psv.value, 'dependent_value', psvd.value)) ",
            "FILTER (WHERE psv.value IS NOT NULL), '[]' ",
        ")::jsonb AS values, ",
        "parm.created_at AS created_at, ",
        "parm.updated_at AS updated_at "
    >>.

common_joins() ->
    <<
        "LEFT OUTER JOIN parameter_value_links AS pvl ON (pvl.parameter_id = parm.id) ",
        "LEFT OUTER JOIN parameter_source_values AS psv ON (psv.id = pvl.value_id) ",
        "LEFT OUTER JOIN parameter_source_values AS psvd ON (psvd.id = pvl.dependent_value_id) "
    >>.
