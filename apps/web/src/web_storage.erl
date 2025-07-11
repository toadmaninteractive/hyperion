-module(web_storage).

-behaviour(gen_server).

%% Include files

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("db/include/protocol.hrl").
-include("settings.hrl").
-include("kv.hrl").

%% Exported functions

-export([
    start_link/0,
    fetch/1,
    store/2,
    delete/1,
    jwk/0,
    username/1
]).

%% gen_server callbacks

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(ets_storage_data, web_storage).
-define(key_jwk, jwk).

-record(state, {}).

%% API

-spec start_link() ->
    {'ok', pid()} | 'ignore' | {'error', {'already_started', pid()} | term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec fetch(Key :: any()) ->
    {'ok', any()} | 'undefined'.

fetch(Key) ->
    case ets:lookup(?ets_storage_data, Key) of
        [#kv{key = Key, value = Value}|_] -> {ok, Value};
        _ -> undefined
    end.

-spec store(Key :: any(), Value :: any()) -> true.

store(Key, Value) ->
    ets:insert(?ets_storage_data, #kv{key = Key, value = Value}).

-spec delete(Key :: any()) -> true.

delete(Key) ->
    ets:delete(?ets_storage_data, Key).

-spec jwk() -> maps:map().

jwk() ->
    {ok, JWK} = fetch(?key_jwk),
    JWK.

-spec username(UserId :: non_neg_integer()) -> binary() | 'undefined'.

username(UserId) ->
    Key = {personnel, UserId},
    case fetch(Key) of
        {ok, Username} when is_binary(Username) ->
            Username;
        undefined ->
            case db_if_personnel:get_one(UserId) of
                {ok, #{<<"username">> := Username}} ->
                    store(Key, Username),
                    Username;
                _ -> undefined
            end
    end.

%% gen_server callbacks
init(_Args) ->
    % Create ETS table and fill it
    ets:new(?ets_storage_data, [set, public, named_table, {keypos, #kv.key}]),
    JwkPath = filename:join([<<"web">>, <<"pem">>, <<"server.key">>]),
    ets:insert(?ets_storage_data, #kv{key = ?key_jwk, value = jwk:load(JwkPath)}),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    logger:debug("unhandled call ~p from ~p~n", [Request, From], #{caption => ?MODULE}),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    logger:debug("unhandled cast ~p~n", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

handle_info(Msg, State) ->
    logger:debug("unhandled info ~p~n", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Local functions
