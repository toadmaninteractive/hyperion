-module(access_app).

-behaviour(application).

%% Exported functions

-export([
    start/2,
    stop/1
]).

%% API

start(_Type, _StartArgs) ->
    access_sup:start_link().

stop(_State) ->
    ok.

%% Local functions
