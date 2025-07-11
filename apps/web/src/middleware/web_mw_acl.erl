-module(web_mw_acl).

-behaviour(cowboy_middleware).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("acl.hrl").
-include("http.hrl").
-include("session.hrl").

%% Exported functions

-export([
    execute/2
]).

%% API

execute(#{?m_session := #session{key = {personnel, _}, user_id = PersonnelId}} = Req, #{handler_opts := #acl{} = ACL} = Env) ->
    Method = cowboy_req:method(Req),
    Bindings = cowboy_req:bindings(Req),
    case is_allowed(Method, Bindings, PersonnelId, ACL) of
        true -> {ok, Req, Env};
        false -> {stop, forbidden(Req)}
    end;

execute(Req, #{handler_opts := #acl{}}) ->
    {stop, forbidden(Req)};

execute(Req, Env) ->
    {ok, Req, Env}.

%% Local functions

forbidden(Req) ->
    cowboy_req:reply(?http_forbidden, #{}, Req).

maybe_int(undefined) -> undefined;
maybe_int(I) when is_integer(I) -> I;
maybe_int(S) when is_list(S) -> list_to_integer(S);
maybe_int(B) when is_binary(B) -> binary_to_integer(B).

maybe_result({ok, Value}) -> Value;
maybe_result(_) -> undefined.

method_role(?get, #acl{get = Role}) -> Role;
method_role(?post, #acl{post = Role}) -> Role;
method_role(?put, #acl{put = Role}) -> Role;
method_role(?patch, #acl{patch = Role}) -> Role;
method_role(?delete, #acl{delete = Role}) -> Role;
method_role(_, _) -> undefined.

is_superadmin(PersonnelId) ->
    case db_if_personnel:is_superadmin(PersonnelId) of
        {ok, true} -> true;
        _ -> false
    end.

is_project_accessible(ProjectId, PersonnelId) ->
    case db_if_projects:is_accessible(ProjectId, PersonnelId) of
        {ok, true} -> true;
        _ -> false
    end.

project_id(Bindings, #acl{project_id = B}) when B =/= undefined ->
    maybe_int(maps:get(B, Bindings, undefined));
project_id(Bindings, #acl{parameter_id = B}) when B =/= undefined ->
    ParamId = maybe_int(maps:get(B, Bindings, undefined)),
    ?yesno(is_integer(ParamId), maybe_result(db_if_parameters:project_id(ParamId)), undefined);
project_id(Bindings, #acl{setup_id = B}) when B =/= undefined ->
    SetupId = maybe_int(maps:get(B, Bindings, undefined)),
    ?yesno(is_integer(SetupId), maybe_result(db_if_setup_steps:project_id(SetupId)), undefined);
project_id(Bindings, #acl{test_case_id = B}) when B =/= undefined ->
    CaseId = maybe_int(maps:get(B, Bindings, undefined)),
    ?yesno(is_integer(CaseId), maybe_result(db_if_test_cases:project_id(CaseId)), undefined);
project_id(Bindings, #acl{test_run_id = B}) when B =/= undefined ->
    RunId = maybe_int(maps:get(B, Bindings, undefined)),
    ?yesno(is_integer(RunId), maybe_result(db_if_test_runs:project_id(RunId)), undefined);
project_id(Bindings, #acl{test_run_item_id = B}) when B =/= undefined ->
    ItemId = maybe_int(maps:get(B, Bindings, undefined)),
    ?yesno(is_integer(ItemId), maybe_result(db_if_test_run_items:project_id(ItemId)), undefined);
project_id(Bindings, #acl{owner = OB, linked_id = OL}) when OB =/= undefined, OL =/= undefined ->
    Owner = maps:get(OB, Bindings, undefined),
    LinkedId = maybe_int(maps:get(OL, Bindings, undefined)),
    Fun = case Owner of
        <<"setup_step">> -> fun db_if_setup_steps:project_id/1;
        <<"test_case">> -> fun db_if_test_cases:project_id/1;
        <<"test_run">> -> fun db_if_test_runs:project_id/1;
        <<"test_run_item">> -> fun db_if_test_run_items:project_id/1;
        _ -> undefined
    end,
    ?yesno(is_function(Fun) andalso is_integer(LinkedId), maybe_result(Fun(LinkedId)), undefined);
project_id(_, _) -> undefined.

is_allowed(Method, Bindings, PersonnelId, ACL) ->
    % Check for local admin option
    IsLocalAdmin = access_config:local_admin(),

    % Check if current user is superadmin
    IsSuperadmin = is_superadmin(PersonnelId),

    % Get project and database
    ProjectId = project_id(Bindings, ACL),

    % Proceed with role check
    case method_role(Method, ACL) of
        % No role check required
        undefined -> true;

        % Superadmin access required
        ?role_superadmin -> IsSuperadmin orelse IsLocalAdmin;

        % Superadmin is allowed to access everyting else
        _ when IsSuperadmin -> true;

        % Local admin is also allowed to access everyting else
        _ when IsLocalAdmin -> true;

        % Consumer
        ?role_consumer when is_integer(ProjectId) -> is_project_accessible(ProjectId, PersonnelId);

        % Catch-up for non-superadmin users
        _ when ProjectId =:= undefined -> false;

        % Everyone else
        MinimalAcceptableRole ->
            case db_if_personnel_roles:get_one(PersonnelId, ProjectId) of
                {ok, #{<<"user_role">> := UserRole, <<"group_roles">> := GroupRoles}} ->
                    UserRole1 = ?yesno(is_binary(UserRole), protocol_db:access_role_from_json(UserRole), ?null),
                    GroupRoles1 = ?yesno(is_map(GroupRoles), [protocol_db:access_role_from_json(R) || #{<<"role">> := R} <- maps:values(GroupRoles)], []),
                    Roles = [R || R <- [UserRole1 | GroupRoles1], R =/= ?null],
                    RoleLevels = [web_acl:role_level(R) || R <- lists:usort(Roles)],
                    MaxLevel = ?yesno(RoleLevels =:= [], 0, lists:max(RoleLevels)),
                    MaxLevel >= web_acl:role_level(MinimalAcceptableRole);
                _ -> false
            end
    end.
