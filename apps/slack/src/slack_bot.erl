-module(slack_bot).

-behaviour(gen_server).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("slack_protocol.hrl").

%% Exported functions

-export([
    start_link/0,
    user_id/1,
    user/1
]).

-define(update_users_msg, update_users).        % Message
-define(update_users_retry_interval, 60000).    % 1 min
-define(update_users_interval, 60 * 60 * 1000). % 1 hour

-define(ets_users, slack_bot_users).
-define(ets_members, slack_bot_members).

%% gen_server callbacks

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    pid_users :: pid() | 'undefined'
}).

-record(kv, {
    key :: term(),
    value :: term()
}).

-record(user, {
    id :: binary(),
    display_name :: binary(),
    real_name :: binary()
}).

%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec user_id(Name) -> Result when
    Name :: binary(),
    Result :: binary().

user_id(Name) ->
    case ets:lookup(?ets_users, slack_util:to_lower_trim(Name)) of
        [#kv{value = UserId} | _] -> UserId;
        _ -> undefined
    end.

-spec user(Id) -> Result when
    Id :: binary(),
    Result :: #user{}.

user(Id) ->
    case ets:lookup(?ets_members, Id) of
        [#user{} = User | _] -> User;
        _ -> undefined
    end.

%% gen_server callbacks

init(_) ->
    erlang:send_after(0, self(), init),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    logger:debug("unhandled call ~p from ~p", [Request, From], #{caption => ?MODULE}),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    logger:debug("unhandled cast ~p", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

handle_info(init, State) ->
    ets:new(?ets_users, [set, public, named_table, {keypos, #kv.key}]),
    ets:new(?ets_members, [set, public, named_table, {keypos, #user.id}]),
    erlang:send_after(0, self(), ?update_users_msg),
    {noreply, State};

handle_info(?update_users_msg, State) ->
    Pid = proc_lib:spawn(fun update_slack_users/0),
    erlang:monitor(process, Pid),
    {noreply, State#state{pid_users = Pid}};

handle_info({'DOWN', _MonitorRef, process, Pid, normal}, #state{pid_users = Pid} = State) ->
    erlang:send_after(?update_users_interval, self(), ?update_users_msg),
    {noreply, State#state{pid_users = undefined}};

handle_info({'DOWN', _MonitorRef, process, Pid, Reason}, #state{pid_users = Pid} = State) ->
    logger:error("failed to get Slack users, reason: ~p", [Reason], #{caption => ?MODULE}),
    erlang:send_after(?update_users_retry_interval, self(), ?update_users_msg),
    {noreply, State#state{pid_users = undefined}};

handle_info(Msg, State) ->
    logger:debug("unhandled info ~p", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Local functions

display_name(Id) ->
    case user(Id) of
        #user{display_name = <<>>, real_name = RealName} -> RealName;
        #user{display_name = DisplayName} -> DisplayName
    end.

real_name(Id) ->
    #user{real_name = RealName} = user(Id),
    RealName.

cleanup_users(SlackUsers) ->
    UserKeys = lists:foldl(fun(#member{profile = #member_profile{display_name = DisplayName, real_name = RealName}}, Acc) ->
        Acc1 = ordsets:add_element(slack_util:to_lower_trim(DisplayName), Acc),
        ordsets:add_element(slack_util:to_lower_trim(RealName), Acc1)
    end, ordsets:new(), SlackUsers),
    ExistingKeys = ets:foldl(fun(#kv{key = Key}, AccSet) -> ordsets:add_element(Key, AccSet) end, ordsets:new(), ?ets_users),
    [begin
        ets:delete(?ets_users, Key),
        logger:info("Slack user key <~s> removed", [Key], #{caption => ?MODULE})
    end || Key <- ordsets:subtract(ExistingKeys, UserKeys)].

cleanup_members(SlackUsers) ->
    MemberKeys = ordsets:from_list([Id || #member{id = Id} <- SlackUsers]),
    ExistingKeys = ets:foldl(fun(#user{id = Id}, AccSet) -> ordsets:add_element(Id, AccSet) end, ordsets:new(), ?ets_members),
    [begin
        ets:delete(?ets_members, Key),
        logger:info("Slack member key <~s> removed", [Key], #{caption => ?MODULE})
    end || Key <- ordsets:subtract(ExistingKeys, MemberKeys)].

create_users(SlackUsers) ->
    [begin
        ?doif(DisplayName =/= <<>>, ets:insert(?ets_users, #kv{key = slack_util:to_lower_trim(DisplayName), value = UserId})),
        ets:insert(?ets_users, #kv{key = slack_util:to_lower_trim(RealName), value = UserId}),
        ets:insert(?ets_members, #user{id = UserId, display_name = DisplayName, real_name = RealName})
    end || #member{id = UserId, profile = #member_profile{display_name = DisplayName, real_name = RealName}} <- SlackUsers].

update_slack_users() ->
    SlackUsers = case slack_api:get_users() of
        #get_users_response{members = Members} -> Members;
        _ -> undefined
    end,
    cleanup_users(SlackUsers),
    cleanup_members(SlackUsers),
    create_users(SlackUsers).
