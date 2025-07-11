-module(web_acl).

%% Include files

-include("acl.hrl").

%% Exported functions

-export([
    role_level/1
]).

%% API

-spec role_level(Role) -> Result when
    Role :: ?role_consumer | ?role_maintainer | ?role_admin | ?role_superadmin | any(),
    Result :: non_neg_integer().

role_level(?role_consumer) -> 1;
role_level(?role_maintainer) -> 2;
role_level(?role_admin) -> 3;
role_level(_) -> 0.

%% Local functions
