-module(db_if_props).

%% Include files

-include_lib("epgsql/include/epgsql.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    % Generic
    prop/1,
    set_prop/2,
    inc_prop/2,
    dec_prop/2,
    del_prop/1,

    % Shortcuts
    db_version/0
]).

%% API

-spec prop(Name :: binary()) ->
    {'ok', Value :: binary()} | {'error', Reason :: atom()}.

prop(Name) ->
    Query = <<"SELECT value FROM props WHERE name = LOWER(TRIM($1))">>,
    case db_query:select_one(Query, [util_binary:to_binary(Name)]) of
        {ok, #{<<"value">> := Value}} -> {ok, Value};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec set_prop(Name :: binary(), Value :: binary() | integer()) ->
    'ok' | {'error', Reason :: atom()}.

set_prop(Name, Value) ->
    Query = <<
        "INSERT INTO props VALUES (LOWER(TRIM($1)), $2) ",
        "ON CONFLICT (name) DO UPDATE SET value = $2"
    >>,
    case db_query:insert(Query, [util_binary:to_binary(Name), Value]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec inc_prop(Name :: binary(), Delta :: integer()) ->
    {'ok', NewValue :: integer()} | {'error', Reason :: atom()}.

inc_prop(Name, Delta) when is_integer(Delta) ->
    Query = <<
        "UPDATE props SET value = (value::integer + $2)::varchar ",
        "WHERE name = LOWER(TRIM($1)) ",
        "RETURNING value::integer"
    >>,
    case db_query:update(Query, [util_binary:to_binary(Name), Delta]) of
        {ok, 0, _, _} ->
            ok = set_prop(Name, Delta),
            Delta;
        {ok, 1, Columns, Rows} ->
            #{<<"value">> := Value} = db_util:result_to_json(Columns, Rows),
            Value;
        {error, Reason} ->
            {error, Reason}
    end.

-spec dec_prop(Name :: binary(), Delta :: integer()) ->
    {'ok', NewValue :: integer()} | {'error', Reason :: atom()}.

dec_prop(Name, Delta) when is_integer(Delta) ->
    inc_prop(Name, -Delta).

-spec del_prop(Name :: binary()) ->
    'ok' | {'error', Reason :: atom()}.

del_prop(Name) ->
    Query = <<"DELETE FROM props WHERE name = LOWER(TRIM($1))">>,
    case db_query:execute(Query, [util_binary:to_binary(Name)]) of
        {ok, _} -> ok;
        {error, #error{codename = Codename}} -> {error, Codename}
    end.

-spec db_version() ->
    {'ok', Version :: non_neg_integer()} | {'error', Reason :: atom()}.

db_version() ->
    case prop(db_version) of
        {ok, Version} -> {ok, binary_to_integer(Version)};
        {error, _Reason} = Error -> Error
    end.

%% Local functions
