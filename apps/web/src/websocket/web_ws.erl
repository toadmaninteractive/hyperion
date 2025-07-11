-module(web_ws).

-behaviour(cowboy_websocket).

%% Include files

-include_lib("kernel/include/logger.hrl").
-include("session.hrl").
-include("protocol_notification.hrl").

%% Exported functions

-export([
    broadcast/1,
    whisper/2,
    disconnect_one/1,
    disconnect_all/0,
    subscribe_personnel/1
]).

%% cowboy_websocket callbacks

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2
]).

-define(ws_idle_timeout, 30000).    % 30 sec
-define(ws_ping_interval, 5000).    % 5 sec

-record(state, {
    session :: session()
}).

%% API

-spec broadcast(Notification :: protocol_notification:notification()) -> 'ok'.

broadcast(Notification) ->
    [send_notification(Pid, Notification) || {Pid, _} <- gproc:lookup_local_properties(websocket)].

-spec whisper(UserId :: non_neg_integer(), Notification :: protocol_notification:notification()) -> 'ok'.

whisper(UserId, Notification) ->
    [send_notification(Pid, Notification) || {Pid, _} <- gproc:lookup_local_properties({user_id, UserId})].

-spec disconnect_one(UserId :: non_neg_integer()) -> 'ok'.

disconnect_one(UserId) ->
    [disconnect(Pid) || {Pid, _} <- gproc:lookup_local_properties({user_id, UserId})].

-spec disconnect_all() -> 'ok'.

disconnect_all() ->
    [disconnect(Pid) || {Pid, _} <- gproc:lookup_local_properties(websocket)].

-spec subscribe_personnel(NotificationKind :: protocol_notification:notification_kind()) -> true.

subscribe_personnel(NotificationKind) ->
    gproc:add_local_property({personnel, NotificationKind}, undefined).

%% cowboy_websocket callbacks

init(#{?m_session := Session = #session{}} = Req, _Opts) ->
    {cowboy_websocket, Req, #state{session = Session}, #{idle_timeout => ?ws_idle_timeout}}.

websocket_init(#state{session = #session{key = {UserType, _SessionId}, user_id = UserId}} = State) ->
    % Register websocket
    gproc:add_local_property(websocket, UserType),
    gproc:add_local_property({user_id, UserId}, undefined),

    % Send hello notification and launch heartbeat cycle
    erlang:send_after(0, self(), {send, text, pack(#hello{})}),
    erlang:send_after(?ws_ping_interval, self(), heartbeat),
    % ?LOG_DEBUG("Websocket connected: user_id = ~p, pid = ~p", [UserId, self()]),
    {[], State}.

websocket_handle({text, Msg}, #state{session = #session{key = {UserType, SessionId}, user_id = UserId}} = State) ->
    Json = web_util:decode_json(Msg),
    Notification = protocol_notification:notification_from_json(Json),
    NotificationKind = element(1, Notification),
    publish(NotificationKind, Notification, UserType, UserId, SessionId),
    {ok, State};
websocket_handle(ping, State) ->
    {ok, State};
websocket_handle(pong, State) ->
    {ok, State};
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info(heartbeat, State) ->
    erlang:send_after(?ws_ping_interval, self(), heartbeat),
    {[ping], State};
websocket_info(ping, State) ->
    {[ping], State};
websocket_info({send, text, Msg}, State) ->
    {[{text, Msg}], State};
websocket_info({send, binary, Msg}, State) ->
    {[{binary, Msg}], State};
websocket_info(disconnect, State) ->
    {stop, State};
websocket_info(_Info, State) ->
    {[], State}.

%% Local functions

pack(Notification) ->
    Json = protocol_notification:notification_to_json(Notification),
    jsx:encode(Json).

send(HandlerPid, Type, Message) ->
    HandlerPid ! {send, Type, Message}.

send_text(Pid, Message) ->
    send(Pid, text, Message).

%send_binary(Pid, Message) ->
%    send(Pid, binary, Message).

send_notification(Pid, Notification) ->
    send_text(Pid, pack(Notification)).

%ping(Pid) ->
%    Pid ! ping.

disconnect(Pid) ->
    Pid ! disconnect.

publish(NotificationKind, Notification, UserType, UserId, SessionId) ->
    % Subscribed processes will get the following message: {{UserType, NotificationKind}, {Notification, UserId, SessionId, WebsocketPid}}
    % Example usage: web_ws:subscribe_personnel(hello)
    Key = {UserType, NotificationKind},
    gproc:send({p, l, Key}, {Key, {Notification, UserId, SessionId, self()}}).
