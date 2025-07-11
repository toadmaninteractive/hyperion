-module(access_ldap).

-behaviour(gen_server).

%% Include files

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("access.hrl").

%% Exported functions

-export([
    start_link/0
]).

%% gen_server callbacks

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(invalidate_cached_accounts_msg, invalidate_cached_accounts).
-define(invalidate_cached_accounts_interval, 10000).                        % msec (10 sec)
-define(invalidate_ldap_users_groups_msg, invalidate_ldap_users_groups).
-define(invalidate_ldap_users_groups_interval, 300000).                     % msec (5 min)
-define(authentication_validity_period, 300).                               % sec (5 min)

-record(state, {
    ldap_update_pid = undefined :: pid() | 'undefined'
}).

%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks

init(_) ->
    self() ! init,
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init, State) ->
    % Create ETS tables
    access_ets:init_accounts(),

    % Launch invalidation timers
    self() ! ?invalidate_cached_accounts_msg,
    self() ! ?invalidate_ldap_users_groups_msg,
    {noreply, State};

handle_info(?invalidate_cached_accounts_msg, State) ->
    % Define match spec and delete outdated accounts
    access_ets:delete_expired_accounts(?authentication_validity_period),
    erlang:send_after(?invalidate_cached_accounts_interval, self(), ?invalidate_cached_accounts_msg),
    {noreply, State};

handle_info(?invalidate_ldap_users_groups_msg, State) ->
    Pid = proc_lib:spawn(fun() ->
        invalidate_users(),
        invalidate_groups()
    end),
    erlang:monitor(process, Pid),
    {noreply, State#state{ldap_update_pid = Pid}};

handle_info({'DOWN', _Monitor, process, Pid, _Reason}, #state{ldap_update_pid = Pid} = State) ->
    % Look like user update process died, launh it again at a given interval
    erlang:send_after(?invalidate_ldap_users_groups_interval, self(), ?invalidate_ldap_users_groups_msg),
    {noreply, State#state{ldap_update_pid = undefined}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Local functions

create_account(Username, Name) ->
    case db_if_personnel:create(Username, Name, ?null, ?null) of
        {ok, _PersonnelId} -> logger:info("Created personnel account ~s", [Username]);
        {error, Reason} -> logger:error("Failed to create personnel account ~s (reason: ~s)", [Username, Reason], #{caption => ?MODULE})
    end.

update_account(PersonnelId, Username, Name) ->
    case db_if_personnel:update(PersonnelId, #{<<"name">> => Name, <<"is_deleted">> => false}) of
        ok -> logger:info("Updated personnel account ~s", [Username]);
        {error, Reason} -> logger:error("Failed to update personnel account ~s (reason: ~s)", [Username, Reason], #{caption => ?MODULE})
    end.

deactivate_account(PersonnelId, Username) ->
    web_session:delete_for(?actor_personnel, PersonnelId),
    case db_if_personnel:deactivate(PersonnelId) of
        ok -> logger:info("Deactivated personnel account ~s", [Username]);
        {error, Reason} -> logger:error("Failed to deactivate personnel account ~s (reason: ~s)", [Username, Reason], #{caption => ?MODULE})
    end.

create_group(Name) ->
    case db_if_personnel_groups:create(Name) of
        {ok, _GroupId} -> logger:info("Created personnel group ~s", [Name]);
        {error, Reason} -> logger:error("Failed to create personnel group ~s (reason: ~s)", [Name, Reason], #{caption => ?MODULE})
    end.

update_group(GroupId, Name) ->
    case db_if_personnel_groups:update(GroupId, #{<<"is_deleted">> => false}) of
        ok -> logger:info("Updated personnel group ~s", [Name]);
        {error, Reason} -> logger:error("Failed to update personnel group ~s (reason: ~s)", [Name, Reason], #{caption => ?MODULE})
    end.

deactivate_group(GroupId, Name) ->
    case db_if_personnel_groups:delete(GroupId) of
        ok -> logger:info("Deactivated personnel group ~s", [Name]);
        {error, Reason} -> logger:error("Failed to deactivate personnel group ~s (reason: ~s)", [Name, Reason], #{caption => ?MODULE})
    end.

create_member(Username, GroupName) ->
    case db_if_personnel_group_membership:create_explicit(Username, GroupName) of
        ok -> logger:info("Added personnel account ~s to group ~s", [Username, GroupName]);
        {error, Reason} -> logger:error("Failed to add personnel account ~s to group ~s (reason: ~s)", [Username, GroupName, Reason], #{caption => ?MODULE})
    end.

delete_member(Username, GroupName) ->
    case db_if_personnel_group_membership:delete_explicit(Username, GroupName) of
        ok -> logger:info("Removed personnel account ~s from group ~s", [Username, GroupName]);
        {error, Reason} -> logger:error("Failed to remove personnel account ~s from group ~s (reason: ~s)", [Username, GroupName, Reason], #{caption => ?MODULE})
    end.

invalidate_users() ->
    AuthRealm = access_config:auth_realm(),
    case cerberus:users(AuthRealm) of
        {ok, AccountMap} ->
            % Convert strings to binaries in account map and extract usernames
            AccountMap1 = maps:fold(fun(K, V, Acc) ->
                K1 = util_binary:trim(util_binary:to_lower(util_binary:to_binary(K))),
                V1 = util_binary:trim(util_binary:to_binary(V)),
                Acc#{K1 => V1}
            end, #{}, AccountMap),
            NewUsers = ordsets:from_list(maps:keys(AccountMap1)),

            % Get existing personnel accounts
            {ok, Accounts} = db_if_personnel:get_all(),
            PersonnelMap = lists:foldl(fun(#{<<"username">> := Username} = Account, Acc) ->
                Username1 = util_binary:trim(Username),
                Acc#{Username1 => Account#{<<"username">> => Username1}}
            end, #{}, Accounts),
            ExistingUsers = ordsets:from_list(maps:keys(PersonnelMap)),

            % Determine personnel accounts to create / update / deactivate
            UsersToCreate = ordsets:subtract(NewUsers, ExistingUsers),
            UsersToUpdate = ordsets:intersection(NewUsers, ExistingUsers),
            UsersToDeactivate = ordsets:subtract(ExistingUsers, NewUsers),

            % Create new users
            [create_account(Username, Name) || {Username, Name} <- maps:to_list(maps:with(UsersToCreate, AccountMap1))],

            % Update existing users
            [begin
                #{<<"id">> := PersonnelId, <<"username">> := Username, <<"name">> := Name, <<"is_deleted">> := IsDeleted} = Account,
                NewName = maps:get(Username, AccountMap1, undefined),
                ?doif(NewName =/= Name orelse IsDeleted, update_account(PersonnelId, Username, NewName))
            end || Account <- maps:values(maps:with(UsersToUpdate, PersonnelMap))],

            % Deactivate deleted users
            [begin
                #{<<"id">> := PersonnelId, <<"username">> := Username, <<"is_deleted">> := IsDeleted} = Account,
                ?doif(not IsDeleted, deactivate_account(PersonnelId, Username))
            end || Account <- maps:values(maps:with(UsersToDeactivate, PersonnelMap))];
        _ -> ignore
    end.

invalidate_groups() ->
    AuthRealm = access_config:auth_realm(),
    case cerberus:groups(AuthRealm) of
        {ok, GroupMap} ->
            % Convert strings to binaries in group map and extract usernames
            GroupMap1 = maps:fold(fun(K, V, Acc) ->
                K1 = util_binary:trim(util_binary:to_lower(util_binary:to_binary(K))),
                V1 = [util_binary:trim(util_binary:to_lower(util_binary:to_binary(M))) || M <- V],
                Acc#{K1 => V1}
            end, #{}, GroupMap),
            NewGroups = ordsets:from_list(maps:keys(GroupMap1)),

            % Get existing personnel groups
            {ok, Groups} = db_if_personnel_groups:get_all(),
            PersonnelGroupMap = lists:foldl(fun(#{<<"name">> := Name} = G, Acc) ->
                Name1 = util_binary:trim(Name),
                Acc#{Name1 => G#{<<"name">> => Name1}}
            end, #{}, Groups),
            ExistingGroups = ordsets:from_list(maps:keys(PersonnelGroupMap)),

            % Determine personnel groups to create / update / deactivate
            GroupsToCreate = ordsets:subtract(NewGroups, ExistingGroups),
            GroupsToUpdate = ordsets:intersection(NewGroups, ExistingGroups),
            GroupsToDeactivate = ordsets:subtract(ExistingGroups, NewGroups),

            % Create new groups
            [create_group(Name) || Name <- GroupsToCreate],

            % Update existing groups
            [begin
                #{<<"id">> := GroupId, <<"name">> := Name, <<"is_deleted">> := IsDeleted} = Group,
                ?doif(IsDeleted, update_group(GroupId, Name))
            end || Group <- maps:values(maps:with(GroupsToUpdate, PersonnelGroupMap))],

            % Deactivate deleted groups
            [begin
                #{<<"id">> := GroupId, <<"name">> := Name, <<"is_deleted">> := IsDeleted} = Group,
                ?doif(not IsDeleted, deactivate_group(GroupId, Name))
            end || Group <- maps:values(maps:with(GroupsToDeactivate, PersonnelGroupMap))],

            % Set superadmin group
            SuperAdminGroup = access_config:admin_group(),
            case db_if_personnel_groups:set_superadmin(SuperAdminGroup) of
                ok -> ignore;
                {error, Reason} -> logger:error("Failed to set personnel group ~s as superadmin (reason: ~s)", [SuperAdminGroup, Reason], #{caption => ?MODULE})
            end,

            % Get current and new membership pairs
            FnFoldMapToPairs = fun(GroupName, Members, AccSet) ->
                lists:foldl(fun(Username, AccSet0) ->
                    Username1 = util_binary:trim(Username),
                    GroupName1 = util_binary:trim(GroupName),
                    ordsets:add_element({Username1, GroupName1}, AccSet0)
                end, AccSet, Members)
            end,

            {ok, MembershipMap} = db_if_personnel_group_membership:get_all(),
            MembershipPairs = maps:fold(FnFoldMapToPairs, ordsets:new(), MembershipMap),
            NewMembershipPairs = maps:fold(FnFoldMapToPairs, ordsets:new(), GroupMap1),

            % Update personnel group membership
            [create_member(Username, GroupName) || {Username, GroupName} <- ordsets:subtract(NewMembershipPairs, MembershipPairs)],
            [delete_member(Username, GroupName) || {Username, GroupName} <- ordsets:subtract(MembershipPairs, NewMembershipPairs)],
            ok;
        _ -> ignore
    end.
