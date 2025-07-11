-module(access_ets).

%% Include files

-include_lib("stdlib/include/ms_transform.hrl").
-include("access.hrl").

%% Exported functions

-export([
    init_accounts/0,
    account/1,
    account_exists/1,
    set_account/1,
    set_account/3,
    delete_all_accounts/0,
    delete_expired_accounts/1
]).

-define(ets_accounts, ldap_accounts).

%% API

-spec init_accounts() -> 'ok'.

init_accounts() ->
    ets:new(?ets_accounts, [set, public, named_table, {keypos, #account.username}]),
    ok.

-spec account(Username :: binary()) ->
    {'ok', account()} | 'undefined'.

account(Username) ->
    LowercaseUsername = util_binary:to_lower(Username),
    case ets:lookup(?ets_accounts, LowercaseUsername) of
        [#account{username = LowercaseUsername} = Account|_] -> {ok, Account};
        _ -> undefined
    end.

-spec account_exists(Username :: binary()) ->
    boolean().

account_exists(Username) ->
    ets:member(?ets_accounts, util_binary:to_lower(Username)).

-spec set_account(Account :: account()) -> 'ok'.

set_account(#account{username = Username} = Account) ->
    ets:insert(?ets_accounts, Account#account{username = util_binary:to_lower(Username), timestamp = time:seconds()}),
    ok.

-spec set_account(Username :: binary(), Password :: binary(), AllowedProjects :: gb_sets:set()) -> 'ok'.

set_account(Username, Password, AllowedProjects) ->
    ets:insert(?ets_accounts, #account{
        username = util_binary:to_lower(Username),
        password = Password,
        allowed_projects = AllowedProjects,
        timestamp = time:seconds()
    }),
    ok.

-spec delete_all_accounts() -> 'ok'.

delete_all_accounts() ->
    ets:delete_all_objects(?ets_accounts),
    ok.

-spec delete_expired_accounts(ValidIntervalSec :: non_neg_integer()) -> 'ok'.

delete_expired_accounts(ValidIntervalSec) ->
    Now = time:seconds(),
    MatchSpecDelete = ets:fun2ms(fun(#account{timestamp = Timestamp}) when Timestamp + ValidIntervalSec < Now -> true end),
    ets:select_delete(?ets_accounts, MatchSpecDelete),
    ok.

%% Local functions
