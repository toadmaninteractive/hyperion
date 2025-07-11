-module(web_init).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("acl.hrl").
-include("settings.hrl").
-include("upload.hrl").

%% Exported functions

-export([
    start_cowboy/4
]).

%% API

start_cowboy(BindIp, BindPort, Acceptors, Secure) ->
    % Get paths, make up
    WebDir = filename:join([filename:absname(""), "web"]),
    SslDir = filename:join([WebDir, "ssl"]),
    FrontendDir = filename:join([WebDir, "frontend"]),
    FrontendDistDir = filename:join([FrontendDir, "dist"]),
    {ok, UploadDir} = web_config:upload_dir(),
    AttachmentDir = filename:join([UploadDir, "attachments"]),

    % Index and favicon
    Index = "index.html",
    FavIcon = "favicon.ico",

    % Routes
    Routes = [
        % REST API: Administration
        {"/api/admin/personnel/:id", web_rest_admin_personnel, #acl{get = ?role_superadmin}},
        {"/api/admin/personnel/username/:username", web_rest_admin_personnel_username, #acl{get = ?role_superadmin}},
        {"/api/admin/personnel/:id/roles", web_rest_admin_personnel_roles, #acl{get = ?role_superadmin}},
        {"/api/admin/personnel/:id/roles/:project", web_rest_admin_personnel_role_set, #acl{put = ?role_superadmin, delete = ?role_superadmin}},
        {"/api/admin/personnels", web_rest_admin_personnels, #acl{get = ?role_superadmin}},
        {"/api/admin/personnel-groups/:id", web_rest_admin_personnel_group, #acl{get = ?role_superadmin}},
        {"/api/admin/personnel-groups/name/:name", web_rest_admin_personnel_group_name, #acl{get = ?role_superadmin}},
        {"/api/admin/personnel-groups/:id/roles", web_rest_admin_personnel_group_roles, #acl{get = ?role_superadmin}},
        {"/api/admin/personnel-groups/:id/roles/:project", web_rest_admin_personnel_group_role_set, #acl{put = ?role_superadmin, delete = ?role_superadmin}},
        {"/api/admin/personnel-groups", web_rest_admin_personnel_groups, #acl{get = ?role_superadmin}},
        {"/api/admin/settings", web_rest_admin_settings, #acl{get = ?role_superadmin, put = ?role_superadmin}},

        % REST API: Attachments
        {"/api/attachments/project/:project_id", web_rest_attachments_project, #acl{get = ?role_consumer, owner = owner, project_id = project_id}},
        {"/api/attachments/:id/links", web_rest_attachment_links, []},
        {"/api/attachments/:id/:owner/:linked_id", web_rest_attachment_unlink, #acl{delete = ?role_consumer, owner = owner, linked_id = linked_id}}, % FIXME: check if can unlink
        {"/api/attachments/:owner/:linked_id", web_rest_attachments, #acl{get = ?role_consumer, owner = owner, linked_id = linked_id}},

        % REST API: Auth
        {"/api/auth/personnel/login", web_rest_auth_personnel_login, []},
        {"/api/auth/personnel/logout", web_rest_auth_personnel_logout, []},
        {"/api/auth/personnel/profile", web_rest_auth_personnel_profile, []},
        {"/api/auth/personnel/status", web_rest_auth_personnel_status, []},

        % REST API: JIRA
        {"/api/jira/auth/:jira_id", web_rest_jira_auth, []},
        {"/api/jira/instances", web_rest_jira_instances, #acl{post = ?role_superadmin}},
        {"/api/jira/instances/:id", web_rest_jira_instance, #acl{put = ?role_superadmin, delete = ?role_superadmin}},
        {"/api/jira/issues/:test_run_item_id", web_rest_jira_issues, #acl{post = ?role_consumer, test_run_item_id = test_run_item_id}},

        % REST API: Parameters
        {"/api/parameters/project/:project_id", web_rest_parameters, #acl{get = ?role_consumer, post = ?role_maintainer, project_id = project_id}},
        {"/api/parameters/:id", web_rest_parameter, #acl{get = ?role_consumer, put = ?role_maintainer, delete = ?role_maintainer, parameter_id = id}},
        {"/api/parameters/:id/value/add", web_rest_parameter_value_add, #acl{post = ?role_maintainer, parameter_id = id}},
        {"/api/parameters/:id/value/remove", web_rest_parameter_value_remove, #acl{put = ?role_maintainer, parameter_id = id}},
        {"/api/parameters/:id/value/rename", web_rest_parameter_value_rename, #acl{put = ?role_maintainer, parameter_id = id}},

        % REST API: Projects
        {"/api/projects", web_rest_projects, #acl{post = ?role_superadmin}},
        {"/api/projects/:id/roles/account", web_rest_project_roles_account, #acl{get = ?role_superadmin}},
        {"/api/projects/:id/roles/group", web_rest_project_roles_group, #acl{get = ?role_superadmin}},
        {"/api/projects/:id/roles/me", web_rest_project_roles_me, []},
        {"/api/projects/:id", web_rest_project, #acl{get = ?role_consumer, put = ?role_maintainer, delete = ?role_superadmin, project_id = id}},

        % REST API: Setup
        {"/api/setup/steps/new/:project_id", web_rest_setup_steps, #acl{post = ?role_maintainer, project_id = project_id}},
        {"/api/setup/steps/:id", web_rest_setup_step, #acl{get = ?role_consumer, put = ?role_maintainer, delete = ?role_maintainer, setup_id = id}},
        {"/api/setup/steps/:id/param/:param_id/link", web_rest_setup_step_param_link, #acl{put = ?role_maintainer, setup_id = id}},
        {"/api/setup/steps/:id/param/unlink", web_rest_setup_step_param_unlink, #acl{put = ?role_maintainer, setup_id = id}},
        {"/api/setup/:project_id", web_rest_setup, #acl{get = ?role_consumer, project_id = project_id}},

        % REST API: Test cases
        {"/api/test/cases/new/:project_id", web_rest_test_cases, #acl{post = ?role_maintainer, project_id = project_id}},
        {"/api/test/cases/:id", web_rest_test_case, #acl{get = ?role_consumer, put = ?role_maintainer, delete = ?role_maintainer, test_case_id = id}},
        {"/api/test/cases/:id/:setup_id/specialize", web_rest_test_case_specialize, #acl{put = ?role_maintainer, test_case_id = id}},
        {"/api/test/cases/:id/:setup_id/despecialize", web_rest_test_case_despecialize, #acl{put = ?role_maintainer, test_case_id = id}},
        {"/api/test/cases/tree/:project_id", web_rest_test_case_tree, #acl{get = ?role_consumer, project_id = project_id}},

        % REST API: Test runs
        {"/api/test/runs/new/:project_id", web_rest_test_runs, #acl{post = ?role_maintainer, project_id = project_id}},
        {"/api/test/runs/:id", web_rest_test_run, #acl{get = ?role_consumer, put = ?role_maintainer, delete = ?role_maintainer, test_run_id = id}},
        {"/api/test/runs/:id/clone", web_rest_test_run_clone, #acl{put = ?role_maintainer, test_run_id = id}},
        {"/api/test/runs/:id/close", web_rest_test_run_close, #acl{put = ?role_maintainer, test_run_id = id}},
        {"/api/test/runs/:id/reopen", web_rest_test_run_reopen, #acl{put = ?role_maintainer, test_run_id = id}},
        {"/api/test/runs/:id/start", web_rest_test_run_start, #acl{put = ?role_maintainer, test_run_id = id}},
        {"/api/test/runs/project/:project_id", web_rest_test_runs_project, #acl{get = ?role_consumer, project_id = project_id}},

        % REST API: Test run items
        {"/api/test/run-items/new/:run_id", web_rest_test_run_items, #acl{post = ?role_maintainer, test_run_id = run_id}},
        {"/api/test/run-items/:id", web_rest_test_run_item, #acl{get = ?role_consumer, put = ?role_maintainer, delete = ?role_maintainer, test_run_item_id = id}},
        {"/api/test/run-items/:id/block", web_rest_test_run_item_block, #acl{put = ?role_consumer, test_run_item_id = id}},
        {"/api/test/run-items/:id/fail", web_rest_test_run_item_fail, #acl{put = ?role_consumer, test_run_item_id = id}},
        {"/api/test/run-items/:id/pass", web_rest_test_run_item_pass, #acl{put = ?role_consumer, test_run_item_id = id}},
        {"/api/test/run-items/:id/reopen", web_rest_test_run_item_reopen, #acl{put = ?role_consumer, test_run_item_id = id}},
        {"/api/test/run-items/:id/start", web_rest_test_run_item_start, #acl{put = ?role_consumer, test_run_item_id = id}},
        {"/api/test/run-items/run/:run_id", web_rest_test_run_items_run, #acl{get = ?role_consumer, test_run_id = run_id}},

        % Upload
        {"/upload/attachments/:owner/:linked_id", web_up_attachments, #upload_config{max_file_size = 128 * 1024 * 1024, target_dir = AttachmentDir}},

        % Download
        {"/download/attachments/[...]", cowboy_static, {dir, AttachmentDir}},

        % Websocket
        {"/ws", web_ws, []},

        % Dashboard
        {"/assets/[...]", cowboy_static, {dir, filename:join([FrontendDistDir, "assets"])}},
        {"/static/[...]", cowboy_static, {dir, filename:join([FrontendDistDir, "static"])}},
        {"/dist/[...]", cowboy_static, {dir, FrontendDistDir}},
        {"/" ++ FavIcon, cowboy_static, {file, filename:join([FrontendDistDir, FavIcon])}},
        {'_', cowboy_static, {file, filename:join([FrontendDistDir, Index])}}
    ],

    % Compile route dispatcher
    Dispatch = cowboy_router:compile([{'_', Routes}]),

    % Define middlewares
    Middlewares = [
        % web_mw_log,
        web_mw_access_control,
        web_mw_no_cache,
        web_mw_authenticate,
        web_mw_authorize,
        % web_mw_websocket,
        cowboy_router,
        web_mw_acl,
        cowboy_handler
    ],

    % Define SSL options
    SslOpts = get_ssl_opt(cacertfile, SslDir) ++ get_ssl_opt(certfile, SslDir) ++ get_ssl_opt(keyfile, SslDir),

    % Define server starter, options and environment
    ServerName = iolist_to_binary(io_lib:format("~s_~s", [hyperion_web, ?yesno(Secure, https, http)])),
    StarterFun = case Secure of true -> fun cowboy:start_tls/3; false -> fun cowboy:start_clear/3 end,
    % Opts = #{ip => BindIp, port => BindPort, num_acceptors => Acceptors},
    Opts = [{ip, BindIp}, {port, BindPort}],
    OptsMaybeWithSsl = ?yesno(Secure, Opts ++ SslOpts, Opts),

    % Define path prefixes
    AuthenticatePrefixes = [
        <<"/api/">>,
        <<"/upload">>,
        <<"/download">>,
        <<"/ws">>
    ],

    AuthorizePrefixes = [
        <<"/api/admin/">>,
        <<"/api/attachments">>,
        <<"/api/parameters">>,
        <<"/api/projects">>,
        <<"/api/setup">>,
        <<"/api/test">>,
        <<"/upload">>,
        <<"/download">>,
        <<"/ws">>
    ],

    % Start server
    ProtocolOpts = #{
        env => #{dispatch => Dispatch, authenticate_prefixes => AuthenticatePrefixes, authorize_prefixes => AuthorizePrefixes},
        middlewares => Middlewares
    },

    StarterFun(ServerName, OptsMaybeWithSsl, ProtocolOpts).

%% Local functions

absolute_or_local(FilePath, LocalSslDir) ->
    case filelib:is_regular(FilePath) of
        true -> FilePath;
        false -> filename:join([LocalSslDir, FilePath])
    end.

get_ssl_opt(Param, LocalSslDir) ->
    Result = case Param of
        cacertfile -> web_config:cacertfile();
        certfile -> web_config:certfile();
        keyfile -> web_config:keyfile();
        _ -> undefined
    end,
    case Result of
        {ok, Value} -> [{Param, absolute_or_local(Value, LocalSslDir)}];
        undefined -> []
    end.
