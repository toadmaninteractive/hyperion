-module(db_if_settings).

%% Include files

-include("protocol.hrl").

%% Exported functions

-export([
    % Generic
    get_one/1,
    get/0,
    set_one/2,
    set/1,

    % Shortcuts
    personnel_session_duration/0,
    client_session_duration/0,
    register_confirm_code_lifetime/0,
    phone_confirm_code_lifetime/0,
    password_reset_confirm_code_lifetime/0
]).

%% API

-spec get_one(Param :: binary() | atom()) ->
    {'ok', Value :: binary() | integer() | float() | boolean()} | {'error', Reason :: atom()}.

get_one(Param) ->
    Query = <<"SELECT type, value FROM settings WHERE param = LOWER(TRIM($1))">>,
    Params = [util_binary:to_binary(Param)],
    case db_query:select_one(Query, Params) of
        {ok, #{<<"type">> := Type, <<"value">> := Value}} -> {ok, convert_settings(Type, Value)};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get() ->
    {'ok', maps:map(Param :: binary(), Value :: binary() | integer() | float() | boolean())} | {'error', Reason :: atom()}.

get() ->
    Query = <<"SELECT param, type, value FROM settings">>,
    case db_query:select(Query, []) of
        {ok, Rows} ->
            Result = lists:foldl(fun(#{<<"param">> := Param, <<"type">> := Type, <<"value">> := Value}, Acc) ->
                Acc#{Param => convert_settings(Type, Value)}
            end, #{}, Rows),
            {ok, Result};
        {error, Reason} ->
            {error, Reason}
    end.

-spec set_one(Param :: binary() | atom(), Value :: binary() | integer() | float()) ->
    'ok' | {'error', Reason :: atom()}.

set_one(Param, Value) ->
    Query = <<"UPDATE settings SET value = $2 WHERE param = LOWER(TRIM($1))">>,
    Params = [util_binary:to_binary(Param), util_binary:to_binary(Value)],
    case db_query:update(Query, Params) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec set(ParamMap :: maps:map(Param :: binary() | atom(), Value :: binary() | integer() | float() | boolean())) ->
    'ok' | {'error', Reason :: atom()}.

set(ParamMap) ->
    {Rows, Params, _} = maps:fold(fun(Param, Value, {AccRows, AccParams, AccIndex}) ->
        Row = util_binary:to_binary(io_lib:format("(LOWER(TRIM($~B)), $~B)", [AccIndex, AccIndex + 1])),
        ParamBin = util_binary:to_binary(Param),
        ValueBin = util_binary:to_binary(Value),
        {[Row | AccRows], [ValueBin, ParamBin | AccParams], AccIndex + 2}
    end, {[], [], 1}, ParamMap),
    ReversedBatch = util_binary:join(lists:reverse(Rows), <<", ">>),
    ReversedParams = lists:reverse(Params),
    Query = <<
        "UPDATE settings AS s SET value = b.value FROM (VALUES ",
            ReversedBatch/binary,
        ") AS b(\"param\", \"value\") WHERE b.param = s.param"
    >>,
    case db_query:update(Query, ReversedParams) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec personnel_session_duration() ->
    {'ok', Duration :: non_neg_integer()} | {'error', Reason :: atom()}.

personnel_session_duration() ->
    get_one(personnel_session_duration).

-spec client_session_duration() ->
    {'ok', Duration :: non_neg_integer()} | {'error', Reason :: atom()}.

client_session_duration() ->
    get_one(client_session_duration).

-spec register_confirm_code_lifetime() ->
    {'ok', Lifetime :: non_neg_integer()} | {'error', Reason :: atom()}.

register_confirm_code_lifetime() ->
    get_one(register_confirm_code_lifetime).

-spec phone_confirm_code_lifetime() ->
    {'ok', Lifetime :: non_neg_integer()} | {'error', Reason :: atom()}.

phone_confirm_code_lifetime() ->
    get_one(phone_confirm_code_lifetime).

-spec password_reset_confirm_code_lifetime() ->
    {'ok', Lifetime :: non_neg_integer()} | {'error', Reason :: atom()}.

password_reset_confirm_code_lifetime() ->
    get_one(password_reset_confirm_code_lifetime).

%% Local functions

convert_settings(<<"string">>, Value) when is_binary(Value) -> Value;
convert_settings(<<"int">>, Value) when is_binary(Value) -> binary_to_integer(Value);
convert_settings(<<"float">>, Value) when is_binary(Value) -> util_math:to_float(Value);
convert_settings(<<"bool">>, <<"true">>) -> true;
convert_settings(<<"bool">>, <<"false">>) -> false.
