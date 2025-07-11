-module(web_mw_no_cache).

-behaviour(cowboy_middleware).

%% Exported functions

-export([
    execute/2
]).

%% API

execute(Req, Env) ->
    case cowboy_req:path(Req) of
        <<"/admin/assets/", _/binary>> -> {ok, Req, Env};
        <<"/admin/static/", _/binary>> -> {ok, Req, Env};
        <<"/admin/dist/", _/binary>> -> {ok, Req, Env};
        _ -> {ok, no_cache(Req), Env}
    end.

%% Local functions

no_cache(Req) ->
    Headers = #{
        <<"cache-control">> => <<"no-cache, no-store, must-revalidate">>,
        <<"pragma">> => <<"no-cache">>,
        <<"expires">> => <<"0">>
    },
    cowboy_req:set_resp_headers(Headers, Req).
