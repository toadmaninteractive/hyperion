-module(web_mw_authorize).

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
    Prefixes = maps:get(authorize_prefixes, Env, []),
    Path = cowboy_req:path(Req),
    case web_util:match_prefix(Path, Prefixes) of
        {ok, _Prefix} ->
            case maps:get(?m_session, Req, undefined) of
                undefined -> {stop, forbidden(Req)};
                _ -> {ok, Req, Env}
            end;
        nomatch ->
            {ok, Req, Env}
    end.

%% Local functions

forbidden(Req) ->
    Body = jsx:encode(#{error => not_authorized}),
    ResponseHeaders = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
    cowboy_req:reply(403, ResponseHeaders, Body, Req).
