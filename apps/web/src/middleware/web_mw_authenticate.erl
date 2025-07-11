-module(web_mw_authenticate).

-behaviour(cowboy_middleware).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("session.hrl").

%% Exported functions

-export([
    execute/2
]).

%% API

execute(Req, Env) ->
    Prefixes = maps:get(authenticate_prefixes, Env, []),
    Path = cowboy_req:path(Req),
    Session = case web_util:match_prefix(Path, Prefixes) of
        {ok, _Prefix} ->
            % Store REST API session data as a meta
            SidFromHeader = cowboy_req:header(?x_session_id, Req, undefined),
            SidFromCookie = proplists:get_value(?x_cookie_sid, cowboy_req:parse_cookies(Req), undefined),
            EncodedSession = case {SidFromHeader, SidFromCookie} of
                {HValue, _} when is_binary(HValue) -> HValue;
                {_, CValue} when is_binary(CValue) -> CValue;
                _ -> undefined
            end,

            case web_session:decode(EncodedSession) of
                % Looks like a valid encoded session
                {ok, S} -> S;

                % Does not Look like a valid encoded session OR no session supplied at all
                {error, invalid} -> undefined;

                % Looks like an expired encoded session
                {error, not_exists} -> undefined;

                % No JWK loaded, crash
                {error, jwk_not_loaded} -> erlang:error(jwk_not_loaded)
            end;
        _ -> undefined
    end,
    {ok, Req#{?m_session => Session}, Env}.

%% Local functions
