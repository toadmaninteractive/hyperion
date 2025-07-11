-module(jira_config).

%% Exported functions

-export([
    base_url/0
]).

%% API

-spec base_url() -> binary().

base_url() ->
    {ok, BaseUrl} = application:get_env(jira, base_url),
    util_binary:to_binary(BaseUrl).

%% Local functions
