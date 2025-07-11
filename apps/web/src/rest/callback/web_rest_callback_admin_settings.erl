-module(web_rest_callback_admin_settings).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("session.hrl").
-include("protocol_data.hrl").

%% Exported functions

-export([
    get_settings/0,
    update_settings/1
]).

%% API

-spec get_settings() ->
    protocol_settings:settings().

get_settings() ->
    {ok, Settings} = db_if_settings:get(),
    protocol_settings:settings_from_json(Settings).

-spec update_settings(UpdateRequest :: protocol_settings:settings_update_request()) ->
    protocol_data:generic_response().

update_settings(UpdateRequest) ->
    Patch = protocol_settings:settings_update_request_to_json(UpdateRequest),
    Predicate = fun(_K, V) -> V =/= ?null end,
    Patch1 = maps:filter(Predicate, Patch),
    Result = case db_if_settings:set(Patch1) of
        ok -> true;
        {error, _Reason} -> false
    end,
    #generic_response{result = Result}.

%% Local functions
