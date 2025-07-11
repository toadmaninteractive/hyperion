-module(access).

%% Include files

-include("access.hrl").

%% Exported functions

-export([
    authenticate/2
]).

%% API

-spec authenticate(Username :: binary(), Password :: binary()) ->
    'ok' | {'reject', RejectReason :: 'unknown_user' | 'invalidCredentials' | atom()} | {'error', ErrorReason :: atom()}.

authenticate(Username, Password) ->
    LowercaseUsername = util_binary:to_lower(Username),
    case access_ets:account(LowercaseUsername) of
        {ok, #account{username = LowercaseUsername, password = Password}} -> ok;
        {ok, #account{username = LowercaseUsername, password = _}} -> {error, invalidCredentials};
        undefined -> invalidate_account(LowercaseUsername, Password)
    end.

%% Local functions

invalidate_account(Username, Password) ->
    LowercaseUsername = util_binary:to_lower(Username),
    case cerberus_authenticate(LowercaseUsername, Password) of
        ok -> access_ets:set_account(LowercaseUsername, Password, []);
        Error -> Error
    end.

cerberus_authenticate(Username, Password) ->
    AuthRealm = access_config:auth_realm(),
    try
        case cerberus:authenticate(AuthRealm, Username, Password) of
            ok -> ok;
            {reject, Reason} -> {reject, Reason};
            {error, Reason} -> {error, Reason}
        end
    catch Type:What:StackTrace ->
        logger:error("Cerberus authentication failed (reason: ~p:~p)", [Type, What], #{caption => ?MODULE, stacktrace => StackTrace}),
        {error, exception}
    end.
