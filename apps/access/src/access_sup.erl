-module(access_sup).

-behaviour(supervisor).

%% Exported functions

-export([
    start_link/0
]).

%% supervisor callbacks

-export([
    init/1
]).

%% API

-spec start_link() -> supervisor:startlink_ret().

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks

init(_Args) ->
    {ok, {{one_for_one, 0, 1}, [
        util_erl:supervisor_spec(access_ldap)
    ]}}.

%% Local functions
