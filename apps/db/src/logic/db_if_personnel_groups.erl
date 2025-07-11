-module(db_if_personnel_groups).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/1,
    get_one_by_name/1,
    get/5,
    get_count/1,
    get_all/0,
    create/1,
    update/2,
    delete/1,
    undelete/1,
    exists/1,
    set_superadmin/1
]).

%% API

-spec get_one(GroupId :: non_neg_integer()) ->
    {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_one(GroupId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM personnel_groups AS perg ",
        (common_joins())/binary,
        "WHERE perg.id = $1 ",
        "GROUP BY perg.id"
    >>,
    case db_query:select_one(Query, [GroupId]) of
        {ok, Group} -> {ok, Group};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_one_by_name(GroupName :: binary()) ->
    {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_one_by_name(GroupName) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM personnel_groups AS perg ",
        (common_joins())/binary,
        "WHERE perg.name = LOWER(TRIM($1)) ",
        "GROUP BY perg.id"
    >>,
    case db_query:select_one(Query, [GroupName]) of
        {ok, Group} -> {ok, Group};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get(Needle, OrderBy, OrderDir, Offset, Limit) -> Result when
    Needle :: binary() | 'undefined',
    OrderBy :: binary(),
    OrderDir :: binary(),
    Offset :: non_neg_integer(), Limit :: non_neg_integer(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get(Needle, OrderBy, OrderDir, Offset, Limit) ->
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<" WHERE strpos(LOWER(perg.name), LOWER($1)) > 0 OR strpos(LOWER(perg.description), LOWER($1)) > 0 ">>, <<>>),
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM personnel_groups AS perg ",
        (common_joins())/binary,
        NeedlePart/binary,
        "GROUP BY perg.id ",
        Filter/binary
    >>,
    Params = ?yesno(HasNeedle, [Needle], []),
    db_query:select(Query, Params).

-spec get_count(Needle) -> Result when
    Needle :: binary() | 'undefined',
    Result :: {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_count(Needle) ->
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<"WHERE strpos(LOWER(perg.name), LOWER($1)) > 0 OR strpos(LOWER(perg.description), LOWER($1)) > 0">>, <<>>),
    Query = <<
        "SELECT COUNT(*)::bigint AS count ",
        "FROM personnel_groups AS perg ",
        NeedlePart/binary
    >>,
    Params = ?yesno(HasNeedle, [Needle], []),
    case db_query:select_one(Query, Params) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_all() ->
    {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_all() ->
    Statement = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM personnel_groups AS perg ",
        (common_joins())/binary,
        "GROUP BY perg.id ",
        "ORDER BY name ASC"
    >>,
    db_query:select(Statement, []).

-spec create(Name) -> Result when
    Name :: binary(),
    Result :: {'ok', GroupId :: non_neg_integer()} | {'error', Reason :: atom()}.

create(Name) ->
    Query = <<"INSERT INTO personnel_groups (name) VALUES (LOWER(TRIM($1))) RETURNING id::bigint">>,
    case db_query:insert(Query, [Name]) of
        {ok, 1, Columns, Rows} ->
            [#{<<"id">> := GroupId}] = db_util:result_to_json(Columns, Rows),
            {ok, GroupId};
        {error, unique_violation} ->
            {error, ?err_already_exists};
        {error, Other} ->
            {error, Other}
    end.

-spec update(GroupId :: non_neg_integer(), Patch :: maps:map()) ->
    'ok' | {'error', Reason :: atom()}.

update(GroupId, Patch) ->
    % Define all possible fields
    Fields = [
        ?mk_mod_lower_trim(name),
        ?mk_mod_trim(description),
        is_deleted
    ],

    % Define auto-set fields
    AutoSetFields = [
        ?mk_mod_inc(rev),
        ?mk_mod_set_now(updated_at)
    ],

    % Perform update
    case db_util:mk_update(<<"personnel_groups">>, <<"id">>, GroupId, Fields, AutoSetFields, Patch) of
        {ok, Query, Params} ->
            case db_query:update(Query, Params) of
                {ok, 1} -> ok;
                {ok, 0} -> {error, ?err_not_exists};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete(GroupId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

delete(GroupId) ->
    update(GroupId, #{<<"is_deleted">> => true}).

-spec undelete(GroupId :: non_neg_integer()) ->
    'ok' | {'error', Reason :: atom()}.

undelete(GroupId) ->
    update(GroupId, #{<<"is_deleted">> => false}).

-spec exists(Name :: binary()) ->
    {'ok', boolean()} | {'error', Reason :: atom()}.

exists(Name) ->
    Query = <<"SELECT COUNT(id)::integer AS count FROM personnel_groups WHERE name = LOWER(TRIM($1))">>,
    case db_query:select_one(Query, [Name]) of
        {ok, #{<<"count">> := Count}} -> {ok, Count > 0};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec set_superadmin(GroupName :: binary()) ->
    'ok' | {'error', Reason :: atom()}.

set_superadmin(GroupName) ->
    Query = <<"SELECT set_superadmin_personnel_group($1) AS result">>,
    case db_query:transaction({Query, [GroupName]}) of
        {ok, #{result := [#{<<"result">> := <<"ok">>}|_]}} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % per   : personnel
    % perg  : personnel_groups
    % pergm : personnel_group_membership
    <<
        "perg.id AS id, ",
        "perg.rev AS rev, ",
        "perg.name AS name, ",
        "CASE COUNT(pergm.personnel_id) WHEN 0 THEN '[]'::jsonb ELSE jsonb_agg(per.username)::jsonb END AS members, ",
        "COUNT(pergm.personnel_id)::integer AS member_count, ",
        "perg.is_superadmin AS is_superadmin, ",
        "perg.is_deleted AS is_deleted, ",
        "perg.created_at AS created_at, ",
        "perg.updated_at AS updated_at "
    >>.

common_joins() ->
    <<
        "LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.group_id = perg.id) ",
        "LEFT OUTER JOIN personnel AS per ON (per.id = pergm.personnel_id) "
    >>.
