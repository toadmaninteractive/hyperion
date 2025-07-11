-module(web_rest_callback_admin_personnel_roles).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("protocol_data.hrl").
-include("protocol_personnel.hrl").
-include("session.hrl").
-include("limits.hrl").

%% Exported functions

-export([
    get_personnel_account_roles/6,
    get_personnel_account_roles_for_project/6,
    set_personnel_account_role/3,
    reset_personnel_account_role/2,
    get_personnel_group_roles/6,
    get_personnel_group_roles_for_project/6,
    set_personnel_group_role/3,
    reset_personnel_group_role/2,
    get_my_roles_for_project/2
]).

%% API

-spec get_personnel_account_roles(Id, Needle, OrderBy, OrderDir, Offset, Limit) -> Result when
    Id :: non_neg_integer(),
    Needle :: binary() | 'undefined',
    OrderBy :: protocol_personnel:personnel_account_role_order_by(),
    OrderDir :: protocol_personnel:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Result :: protocol_data:collection_slice(protocol_personnel:personnel_account_role()).

get_personnel_account_roles(Id, Needle, OrderBy, OrderDir, Offset, Limit) ->
    {ok, Roles} = db_if_personnel_roles:get_for_account(Id, Needle, util_binary:to_binary(OrderBy), util_binary:to_binary(OrderDir), Offset, ?slice_limit(Limit)),
    {ok, Total} = db_if_personnel_roles:get_for_account_count(Id, Needle),
    Roles1 = lists:map(fun protocol_db:personnel_account_role_from_json/1, Roles),
    #collection_slice{items = Roles1, total = Total}.

-spec get_personnel_account_roles_for_project(ProjectId, Needle, OrderBy, OrderDir, Offset, Limit) -> Result when
    ProjectId :: non_neg_integer(),
    Needle :: binary() | 'undefined',
    OrderBy :: protocol_personnel:personnel_account_role_order_by(),
    OrderDir :: protocol_personnel:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Result :: protocol_data:collection_slice(protocol_personnel:personnel_account_role()).

get_personnel_account_roles_for_project(ProjectId, Needle, OrderBy, OrderDir, Offset, Limit) ->
    {ok, Roles} = db_if_personnel_roles:get_for_project(ProjectId, Needle, util_binary:to_binary(OrderBy), util_binary:to_binary(OrderDir), Offset, ?slice_limit(Limit)),
    {ok, Total} = db_if_personnel_roles:get_for_project_count(ProjectId, Needle),
    Roles1 = lists:map(fun protocol_db:personnel_account_role_from_json/1, Roles),
    #collection_slice{items = Roles1, total = Total}.

-spec set_personnel_account_role(Request, Id, ProjectId) -> Result when
    Request :: protocol_personnel:access_role_update_request(),
    Id :: non_neg_integer(),
    ProjectId :: non_neg_integer(),
    Result :: protocol_personnel:generic_response().

set_personnel_account_role(#access_role_update_request{role = Role}, Id, ProjectId) ->
    Result = db_if_personnel_roles:set(Id, ProjectId, Role) =:= ok,
    #generic_response{result = Result}.

-spec reset_personnel_account_role(Id, ProjectId) -> Result when
    Id :: non_neg_integer(),
    ProjectId :: non_neg_integer(),
    Result :: protocol_personnel:generic_response().

reset_personnel_account_role(Id, ProjectId) ->
    Result = case db_if_personnel_roles:delete(Id, ProjectId) of
        ok -> true;
        {error, ?err_not_exists} -> true;
        {error, _Reason} -> false
    end,
    #generic_response{result = Result}.

-spec get_personnel_group_roles(Id, Needle, OrderBy, OrderDir, Offset, Limit) -> Result when
    Id :: non_neg_integer(),
    Needle :: binary() | 'undefined',
    OrderBy :: protocol_personnel:personnel_group_role_order_by(),
    OrderDir :: protocol_personnel:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Result :: protocol_data:collection_slice(protocol_personnel:personnel_group_role()).

get_personnel_group_roles(Id, Needle, OrderBy, OrderDir, Offset, Limit) ->
    {ok, Roles} = db_if_personnel_group_roles:get_for_group(Id, Needle, util_binary:to_binary(OrderBy), util_binary:to_binary(OrderDir), Offset, ?slice_limit(Limit)),
    {ok, Total} = db_if_personnel_group_roles:get_for_group_count(Id, Needle),
    Roles1 = lists:map(fun protocol_db:personnel_group_role_from_json/1, Roles),
    #collection_slice{items = Roles1, total = Total}.

-spec get_personnel_group_roles_for_project(ProjectId, Needle, OrderBy, OrderDir, Offset, Limit) -> Result when
    ProjectId :: non_neg_integer(),
    Needle :: binary() | 'undefined',
    OrderBy :: protocol_personnel:personnel_group_role_order_by(),
    OrderDir :: protocol_personnel:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Result :: protocol_data:collection_slice(protocol_personnel:personnel_group_role()).

get_personnel_group_roles_for_project(ProjectId, Needle, OrderBy, OrderDir, Offset, Limit) ->
    {ok, Roles} = db_if_personnel_group_roles:get_for_project(ProjectId, Needle, util_binary:to_binary(OrderBy), util_binary:to_binary(OrderDir), Offset, ?slice_limit(Limit)),
    {ok, Total} = db_if_personnel_group_roles:get_for_project_count(ProjectId, Needle),
    Roles1 = lists:map(fun protocol_db:personnel_group_role_from_json/1, Roles),
    #collection_slice{items = Roles1, total = Total}.

-spec set_personnel_group_role(Request, Id, ProjectId) -> Result when
    Request :: protocol_personnel:access_role_update_request(),
    Id :: non_neg_integer(),
    ProjectId :: non_neg_integer(),
    Result :: protocol_personnel:generic_response().

set_personnel_group_role(#access_role_update_request{role = Role}, Id, ProjectId) ->
    Result = db_if_personnel_group_roles:set(Id, ProjectId, Role) =:= ok,
    #generic_response{result = Result}.

-spec reset_personnel_group_role(Id, ProjectId) -> Result when
    Id :: non_neg_integer(),
    ProjectId :: non_neg_integer(),
    Result :: protocol_personnel:generic_response().

reset_personnel_group_role(Id, ProjectId) ->
    Result = case db_if_personnel_group_roles:delete(Id, ProjectId) of
        ok -> true;
        {error, ?err_not_exists} -> true;
        {error, _Reason} -> false
    end,
    #generic_response{result = Result}.

-spec get_my_roles_for_project(ProjectId, Req) -> Result when
    ProjectId :: non_neg_integer(),
    Req :: cowboy_req:req(),
    Result :: {protocol_personnel:personnel_account_role(), cowboy_req:req()}.

get_my_roles_for_project(ProjectId, #{?m_session := #session{key = {?actor_personnel, _}, user_id = UserId}} = Req) ->
    {ok, Json} = db_if_personnel_roles:get_one(UserId, ProjectId),
    AccountRole = protocol_db:personnel_account_role_from_json(Json),
    {AccountRole, Req}.

%% Local functions
