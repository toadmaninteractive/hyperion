-module(db_if_test_cases).

%% Include files

-include_lib("epgsql/include/epgsql.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/1,
    get_children/1,
    get_all/1,
    create/11,
    update/2,
    update/3,
    delete/1,
    exists/1,
    project_id/1,
    parent_id/1,
    setup_id/1,
    specialize/4,
    despecialize/2
]).

%% API

-spec get_one(CaseId) -> Result when
    CaseId :: non_neg_integer(),
    Result :: {'ok', protocol_db:test_case()} | {'error', Reason :: atom()}.

get_one(CaseId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM test_cases AS tc ",
        (common_joins())/binary,
        "WHERE tc.id = $1 ",
        "GROUP BY tc.id "
    >>,
    case db_query:select_one(Query, [CaseId]) of
        {ok, Item} -> {ok, protocol_db:test_case_from_json(Item)};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_children(CaseId :: non_neg_integer()) ->
    {'ok', Items :: [protocol_db:test_case()]} | {'error', Reason :: atom()}.

get_children(CaseId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM test_cases AS tc ",
        (common_joins())/binary,
        "WHERE tc.parent_id = $1 ",
        "GROUP BY tc.id ",
        "ORDER BY order_num ASC, LOWER(title) ASC "
    >>,
    case db_query:select(Query, [CaseId]) of
        {ok, Items} -> {ok, [protocol_db:test_case_from_json(Item) || Item <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec get_all(ProjectId :: non_neg_integer()) ->
    {'ok', Items :: [protocol_db:test_case()]} | {'error', Reason :: atom()}.

get_all(ProjectId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM test_cases AS tc ",
        (common_joins())/binary,
        "WHERE tc.project_id = $1 ",
        "GROUP BY tc.id ",
        "ORDER BY order_num ASC, LOWER(title) ASC "
    >>,
    case db_query:select(Query, [ProjectId]) of
        {ok, Items} -> {ok, [protocol_db:test_case_from_json(Item) || Item <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec create(ParentId, ProjectId, PreconditionId, SetupId, IsGroup, Title, Description, TestSteps, ExpectedResult, IsDraft, OrderNum) -> Result when
    ParentId :: non_neg_integer() | 'null',
    ProjectId :: non_neg_integer(),
    PreconditionId :: non_neg_integer() | 'null',
    SetupId :: non_neg_integer() | 'null',
    IsGroup :: boolean(),
    Title :: binary() | 'null',
    Description :: binary() | 'null',
    TestSteps :: binary(),
    ExpectedResult :: binary(),
    IsDraft :: boolean(),
    OrderNum :: integer(),
    Result :: {'ok', CaseId :: non_neg_integer()} | {'error', Reason :: atom()}.

create(ParentId, ProjectId, PreconditionId, SetupId, IsGroup, Title, Description, TestSteps, ExpectedResult, IsDraft, OrderNum) ->
    Query = <<
        "INSERT INTO test_cases (",
            "parent_id, ",
            "project_id, ",
            "precondition_id, ",
            "setup_id, ",
            "is_group, ",
            "title, ",
            "description, ",
            "test_steps, ",
            "expected_result, ",
            "is_draft, ",
            "order_num"
        ") ",
        "VALUES ($1, $2, $3, $4, $5, TRIM($6), TRIM($7), TRIM($8), TRIM($9), $10, $11) ",
        "RETURNING id::bigint"
    >>,
    Params = [ParentId, ProjectId, PreconditionId, SetupId, IsGroup, Title, Description, TestSteps, ExpectedResult, IsDraft, OrderNum],
    case db_query:insert(Query, Params, [raw_error]) of
        {ok, 1, Columns, Rows} ->
            [#{<<"id">> := CaseId}] = db_util:result_to_json(Columns, Rows),
            {ok, CaseId};
        {error, #error{codename = unique_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"tc_project_case_title_ult_index">> -> {error, title_already_exists};
                <<"tc_project_case_title_null_parent_index">> -> {error, title_already_exists};
                <<"tc_project_case_title_null_title_index">> -> {error, title_already_exists}
            end;
        {error, #error{codename = foreign_key_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"test_cases_parent_id_fkey">> -> {error, parent_not_exists};
                <<"test_cases_project_id_fkey">> -> {error, project_not_exists};
                <<"test_cases_precondition_id_fkey">> -> {error, precondition_not_exists};
                <<"test_cases_setup_id_fkey">> -> {error, setup_not_exists}
            end;
        {error, #error{codename = Code}} ->
            {error, Code}
    end.

-spec update(CaseId, Patch) -> Result when
    CaseId :: non_neg_integer(),
    Patch :: maps:map(),
    Result :: 'ok' | {'error', Reason :: atom()}.

update(CaseId, Patch) ->
    update(CaseId, undefined, Patch).

-spec update(CaseId, Rev, Patch) -> Result when
    CaseId :: non_neg_integer(),
    Rev :: non_neg_integer() | 'undefined',
    Patch :: maps:map(),
    Result :: 'ok' | {'error', Reason :: atom()}.

update(CaseId, Rev, Patch) ->
    % Define all possible fields
    Fields = [
        parent_id,
        precondition_id,
        setup_id,
        ?mk_mod_trim(title),
        ?mk_mod_trim(description),
        ?mk_mod_trim(test_steps),
        ?mk_mod_trim(expected_result),
        is_draft,
        order_num
    ],

    % Define auto-set fields
    AutoSetFields = [
        ?mk_mod_inc(rev),
        ?mk_mod_set_now(updated_at)
    ],

    % Perform update
    case db_util:mk_update(<<"test_cases">>, <<"id">>, CaseId, <<"rev">>, Rev, Fields, AutoSetFields, Patch) of
        {ok, Query, Params} ->
            case db_query:update(Query, Params, [raw_error]) of
                {ok, 1} -> ok;
                {ok, 0} -> {error, ?err_not_exists};
                {error, #error{codename = unique_violation, extra = ExtraProps}} ->
                    case proplists:get_value(constraint_name, ExtraProps, undefined) of
                        <<"tc_project_case_title_ult_index">> -> {error, title_already_exists};
                        <<"tc_project_case_title_null_parent_index">> -> {error, title_already_exists};
                        <<"tc_project_case_title_null_title_index">> -> {error, title_already_exists}
                    end;
                {error, #error{codename = foreign_key_violation, extra = ExtraProps}} ->
                    case proplists:get_value(constraint_name, ExtraProps, undefined) of
                        <<"test_cases_parent_id_fkey">> -> {error, parent_not_exists};
                        <<"test_cases_project_id_fkey">> -> {error, project_not_exists};
                        <<"test_cases_precondition_id_fkey">> -> {error, precondition_not_exists};
                        <<"test_cases_setup_id_fkey">> -> {error, setup_not_exists}
                    end;
                {error, #error{codename = Code}} ->
                    {error, Code}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete(CaseId) -> Result when
    CaseId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete(CaseId) ->
    Query = <<"DELETE FROM test_cases WHERE id = $1">>,
    case db_query:delete(Query, [CaseId], [raw_error]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, #error{codename = foreign_key_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"test_cases_parent_id_fkey">> -> {error, has_children}
            end;
        {error, #error{codename = Code}} ->
            {error, Code}
    end.

-spec exists(CaseId) -> Result when
    CaseId :: non_neg_integer(),
    Result :: {'ok', boolean()} | {'error', Reason :: atom()}.

exists(CaseId) ->
    Query = <<"SELECT COUNT(*)::bigint > 0 AS result FROM test_cases WHERE id = $1">>,
    case db_query:select_one(Query, [CaseId]) of
        {ok, #{<<"result">> := Result}} -> {ok, Result};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec project_id(CaseId) -> Result when
    CaseId :: non_neg_integer(),
    Result :: {'ok', non_neg_integer()} | {'error', Reason :: atom()}.

project_id(CaseId) ->
    Query = <<"SELECT project_id FROM test_cases WHERE id = $1">>,
    case db_query:select_one(Query, [CaseId]) of
        {ok, #{<<"project_id">> := ProjectId}} -> {ok, ProjectId};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec parent_id(CaseId) -> Result when
    CaseId :: non_neg_integer(),
    Result :: {'ok', non_neg_integer() | 'undefined'} | {'error', Reason :: atom()}.

parent_id(CaseId) ->
    Query = <<"SELECT parent_id FROM test_cases WHERE id = $1">>,
    case db_query:select_one(Query, [CaseId]) of
        {ok, #{<<"parent_id">> := ?null}} -> {ok, undefined};
        {ok, #{<<"parent_id">> := ParentId}} -> {ok, ParentId};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec setup_id(CaseId) -> Result when
    CaseId :: non_neg_integer(),
    Result :: {'ok', non_neg_integer() | 'undefined'} | {'error', Reason :: atom()}.

setup_id(CaseId) ->
    Query = <<"SELECT setup_id FROM test_cases WHERE id = $1">>,
    case db_query:select_one(Query, [CaseId]) of
        {ok, #{<<"setup_id">> := ?null}} -> {ok, undefined};
        {ok, #{<<"setup_id">> := SetupId}} -> {ok, SetupId};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec specialize(CaseId, SetupId, SpecType, TargetId) -> Result when
    CaseId :: non_neg_integer(),
    SetupId :: non_neg_integer(),
    SpecType :: atom(),
    TargetId :: non_neg_integer() | 'undefined',
    Result :: 'ok' | {'error', Reason :: atom()}.

specialize(SetupId, ParamId, SpecType, TargetId) ->
    Query = <<"SELECT specialize_test_case_setup($1, $2, $3, $4) AS result">>,
    Params = [SetupId, ParamId, SpecType, TargetId],
    case db_query:transaction({Query, Params}) of
        {ok, #{result := [#{<<"result">> := <<"ok">>}|_]}} -> ok;
        {ok, #{result := [#{<<"result">> := <<"test_case_not_exists">>}|_]}} -> {error, test_case_not_exists};
        {ok, #{result := [#{<<"result">> := <<"setup_step_not_assigned">>}|_]}} -> {error, setup_step_not_assigned};
        {ok, #{result := [#{<<"result">> := <<"setup_step_not_exists">>}|_]}} -> {error, setup_step_not_exists};
        {ok, #{result := [#{<<"result">> := <<"invalid_setup_step">>}|_]}} -> {error, invalid_setup_step};
        {ok, #{result := [#{<<"result">> := <<"parameterless_setup_step">>}|_]}} -> {error, parameterless_setup_step};
        {ok, #{result := [#{<<"result">> := <<"invalid_parameter_value">>}|_]}} -> {error, invalid_parameter_value};
        {ok, #{result := [#{<<"result">> := <<"invalid_parameter_source">>}|_]}} -> {error, invalid_parameter_source};
        {error, Reason} -> {error, Reason}
    end.

-spec despecialize(CaseId, SetupId) -> Result when
    CaseId :: non_neg_integer(),
    SetupId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

despecialize(CaseId, SetupId) ->
    Query = <<"DELETE FROM parameter_setup_specialization WHERE case_id = $1 AND setup_id = $2">>,
    Params = [CaseId, SetupId],
    case db_query:delete(Query, Params, [raw_error]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, #error{codename = Code}} -> {error, Code}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % tc  : test_cases
    % pss : parameter_setup_specialization
    % psv : parameter_source_values
    <<
        "tc.id AS id, ",
        "tc.rev AS rev, ",
        "tc.parent_id AS parent_id, ",
        "tc.project_id AS project_id, ",
        "tc.precondition_id AS precondition_id, ",
        "tc.setup_id AS setup_id, ",
        "tc.is_group AS is_group, ",
        "tc.title AS title, ",
        "tc.description AS description, ",
        "tc.test_steps AS test_steps, ",
        "tc.expected_result AS expected_result, ",
        "tc.is_draft AS is_draft, ",
        "tc.order_num AS order_num, ",
        "COALESCE(",
            "jsonb_agg(jsonb_build_object('setup_id', pss.setup_id, 'spec_type', pss.spec_type, 'value', psv.value, 'parameter_id', pss.parameter_id)) ",
            "FILTER (WHERE pss.case_id IS NOT NULL AND pss.setup_id IS NOT NULL), '[]' ",
        ")::jsonb AS specs, ",
        "tc.created_at AS created_at, ",
        "tc.updated_at AS updated_at "
    >>.

common_joins() ->
    <<
        "LEFT OUTER JOIN parameter_setup_specialization AS pss ON (pss.case_id = tc.id) ",
        "LEFT OUTER JOIN parameter_source_values AS psv ON (psv.id = pss.value_id) "
    >>.
