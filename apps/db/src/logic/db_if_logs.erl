-module(db_if_logs).

%% Include files

-include("protocol.hrl").

%% Exported functions

-export([
    get_one/1,
    get/4,
    get_count/0,
    create/7
]).

%% API

-spec get_one(LogId :: binary()) ->
    {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_one(LogId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM logs AS l ",
        (common_joins())/binary,
        "WHERE l.id = $1"
    >>,
    case db_query:select_one(Query, [LogId]) of
        {ok, Session} -> {ok, Session};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get(OrderBy, OrderDir, Offset, Limit) -> Result when
    OrderBy :: binary(),
    OrderDir :: binary(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get(OrderBy, OrderDir, Offset, Limit) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM logs AS l ",
        (common_joins())/binary
    >>,
    Filter = db_util:mk_query_filter(OrderBy, <<"l">>, OrderDir, Offset, Limit),
    Statement = <<Query/binary, Filter/binary>>,
    db_query:select(Statement, []).

-spec get_count() ->
    {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_count() ->
    Query = <<"SELECT COUNT(id)::bigint AS count FROM logs">>,
    case db_query:select_one(Query, []) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec create(Actor, ActorId, Entity, EntityId, Operation, Properties, OpResult) -> Result when
    Actor :: binary() | atom(),
    ActorId :: non_neg_integer() | 'null',
    Entity :: binary() | atom(),
    EntityId :: non_neg_integer() | 'null',
    Operation  :: binary() | atom(),
    Properties :: jsx:json_term(),
    OpResult :: boolean(),
    Result :: 'ok' | {'error', Reason :: atom()}.

create(Actor, ActorId, Entity, EntityId, Operation, Properties, OpResult) ->
    Query = <<
        "INSERT INTO logs (actor, actor_id, entity, entity_id, operation, properties, result) ",
        "VALUES ($1, $2, $3, $4, $5, $6, $7)"
    >>,
    Params = [
        util_binary:to_binary(Actor),
        ActorId,
        util_binary:to_binary(Entity),
        EntityId,
        util_binary:to_binary(Operation),
        jsx:encode(Properties),
        OpResult
    ],
    case db_query:insert(Query, Params) of
        {ok, 1} -> ok;
        {error, Other} -> {error, Other}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % l : logs
    % per : personnel
    Root = <<"Root">>,
    Robot = <<"Robot">>,
    Unknown = <<"Unknown">>,
    <<
        "l.id AS id, ",
        "l.actor AS actor, ",
        "l.actor_id AS actor_id, ",
        "CASE ",
            "WHEN l.actor='", (atom_to_binary(?actor_root, latin1))/binary, "' THEN '", Root/binary, "' ",
            "WHEN l.actor='", (atom_to_binary(?actor_personnel, latin1))/binary, "' AND l.actor_id IS NULL THEN '", Root/binary, "' ",
            "WHEN l.actor='", (atom_to_binary(?actor_personnel, latin1))/binary, "' THEN per.username ",
            "WHEN l.actor='", (atom_to_binary(?actor_robot, latin1))/binary, "' THEN '", Robot/binary, "' ",
            "ELSE '", Unknown/binary, "' ",
        "END as actor_name,"
        "l.entity AS entity, ",
        "l.entity_id AS entity_id, ",
        "l.operation AS operation, ",
        "l.properties AS properties, ",
        "l.result AS result, ",
        "l.created_at AS created_at "
    >>.

common_joins() ->
    <<
       "LEFT OUTER JOIN personnel AS per ON (l.actor_id = per.id) "
    >>.
