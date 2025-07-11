-module(db_if_personnel_roles).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/2,
    get_for_account/6,
    get_for_account_count/2,
    get_for_project/6,
    get_for_project_count/2,
    set/3,
    delete/2,
    delete_for_account/1,
    delete_for_project/1
]).

%% API

-spec get_one(PersonnelId, ProjectId) -> Result when
    PersonnelId :: non_neg_integer(),
    ProjectId :: non_neg_integer(),
    Result :: {'ok', jsx:json_term()} | {'error', Reason :: atom()}.

get_one(PersonnelId, ProjectId) ->
    Query = <<
        "SELECT ",
            "per.id AS personnel_id, ",
            "per.username AS username, ",
            "proj.id AS project_id, ",
            "proj.title AS project_title, ",
            "perr.role AS user_role, ",
            "COALESCE((",
                "SELECT jsonb_object_agg(t.name, jsonb_build_object('role', t.role)) ",
                "FROM ( ",
                    "SELECT pg.name, pgr.role ",
                    "FROM personnel_group_roles AS pgr ",
                    "LEFT OUTER JOIN personnel_groups AS pg ON (pg.id = pgr.group_id) ",
                    "LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.personnel_id = $1) ",
                    "WHERE pgr.project_id = proj.id AND pgr.group_id = pergm.group_id ",
                    "GROUP BY pg.name, pgr.role ",
                ") AS t ",
            "), '{}'::jsonb) AS group_roles ",
        "FROM projects AS proj ",
        "LEFT OUTER JOIN personnel AS per ON (per.id = $1) ",
        "LEFT OUTER JOIN personnel_roles AS perr ON (perr.project_id = proj.id AND perr.personnel_id = $1) ",
        "WHERE proj.id = $2"
    >>,
    case db_query:select_one(Query, [PersonnelId, ProjectId]) of
        {ok, AccountRole} -> {ok, AccountRole};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_for_account(PersonnelId, Needle, OrderBy, OrderDir, Offset, Limit) -> Result when
    PersonnelId :: binary(),
    Needle :: binary() | 'undefined',
    OrderBy :: binary(),
    OrderDir :: binary(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_for_account(PersonnelId, Needle, OrderBy, OrderDir, Offset, Limit) ->
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<" WHERE (strpos(LOWER(proj.id), LOWER($2)) > 0 OR strpos(LOWER(proj.title), LOWER($2)) > 0)">>, <<>>),
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    Query = <<
        "SELECT ",
            "per.id AS personnel_id, ",
            "per.username AS username, ",
            "proj.id AS project_id, ",
            "proj.title AS project_title, ",
            "perr.role AS user_role, ",
            "COALESCE((",
                "SELECT jsonb_object_agg(t.name, jsonb_build_object('role', t.role)) ",
                "FROM ( ",
                    "SELECT pg.name, pgr.role ",
                    "FROM personnel_group_roles AS pgr ",
                    "LEFT OUTER JOIN personnel_groups AS pg ON (pg.id = pgr.group_id) ",
                    "LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.personnel_id = $1) ",
                    "WHERE pgr.project_id = proj.id AND pgr.group_id = pergm.group_id ",
                    "GROUP BY pg.name, pgr.role ",
                ") AS t ",
            "), '{}'::jsonb) AS group_roles ",
        "FROM projects AS proj ",
        "LEFT OUTER JOIN personnel AS per ON (per.id = $1) ",
        "LEFT OUTER JOIN personnel_roles AS perr ON (perr.project_id = proj.id AND perr.personnel_id = $1) ",
        NeedlePart/binary,
        Filter/binary
    >>,
    Params = [PersonnelId] ++ ?yesno(HasNeedle, [Needle], []),
    db_query:select(Query, Params).

-spec get_for_account_count(PersonnelId, Needle) -> Result when
    PersonnelId :: binary(),
    Needle :: binary() | 'undefined',
    Result :: {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_for_account_count(PersonnelId, Needle) ->
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<" WHERE (strpos(LOWER(proj.id), LOWER($2)) > 0 OR strpos(LOWER(proj.title), LOWER($2)) > 0)">>, <<>>),
    Query = <<
        "SELECT COUNT(*)::bigint AS count ",
        "FROM projects AS proj ",
        "LEFT OUTER JOIN personnel_roles AS perr ON (perr.project_id = proj.id AND perr.personnel_id = $1) ",
        NeedlePart/binary
    >>,
    Params = [PersonnelId] ++ ?yesno(HasNeedle, [Needle], []),
    case db_query:select_one(Query, Params) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_for_project(ProjectId, Needle, OrderBy, OrderDir, Offset, Limit) -> Result when
    ProjectId :: non_neg_integer(),
    Needle :: binary() | 'undefined',
    OrderBy :: binary(),
    OrderDir :: binary(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_for_project(ProjectId, Needle, OrderBy, OrderDir, Offset, Limit) ->
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<" WHERE (strpos(LOWER(per.username), LOWER($2)) > 0 OR strpos(LOWER(per.name), LOWER($2)) > 0)">>, <<>>),
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    Query = <<
        "SELECT ",
            "per.id AS personnel_id, ",
            "per.username AS username, ",
            "proj.id AS project_id, ",
            "proj.title AS project_title, ",
            "perr.role AS user_role, ",
            "COALESCE((",
                "SELECT jsonb_object_agg(t.name, jsonb_build_object('role', t.role)) ",
                "FROM ( ",
                    "SELECT pg.name, pgr.role ",
                    "FROM personnel_group_roles AS pgr ",
                    "LEFT OUTER JOIN personnel_groups AS pg ON (pg.id = pgr.group_id) ",
                    "LEFT OUTER JOIN personnel_group_membership AS pergm ON (pergm.personnel_id = per.id) ",
                    "WHERE pgr.project_id = $1 AND pgr.group_id = pergm.group_id ",
                    "GROUP BY pg.name, pgr.role ",
                ") AS t ",
            "), '{}'::jsonb) AS group_roles ",
        "FROM personnel AS per ",
        "LEFT OUTER JOIN projects AS proj ON (proj.id = $1) ",
        "LEFT OUTER JOIN personnel_roles AS perr ON (perr.personnel_id = per.id AND perr.project_id = $1) ",
        NeedlePart/binary,
        Filter/binary
    >>,
    Params = [ProjectId] ++ ?yesno(HasNeedle, [Needle], []),
    db_query:select(Query, Params).

-spec get_for_project_count(ProjectId, Needle) -> Result when
    ProjectId :: non_neg_integer(),
    Needle :: binary() | 'undefined',
    Result :: {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_for_project_count(ProjectId, Needle) ->
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<" WHERE (strpos(LOWER(per.username), LOWER($2)) > 0 OR strpos(LOWER(per.name), LOWER($2)) > 0)">>, <<>>),
    Query = <<
        "SELECT COUNT(*)::bigint AS count ",
        "FROM personnel AS per ",
        "LEFT OUTER JOIN personnel_roles AS perr ON (perr.personnel_id = per.id AND perr.project_id = $1) ",
        NeedlePart/binary
    >>,
    Params = [ProjectId] ++ ?yesno(HasNeedle, [Needle], []),
    case db_query:select_one(Query, Params) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec set(PersonnelId, ProjectId, Role) -> Result when
    PersonnelId :: non_neg_integer(),
    ProjectId :: non_neg_integer(),
    Role :: atom(),
    Result :: 'ok' |  {'error', Reason :: atom()}.

set(PersonnelId, ProjectId, Role) ->
    Query = <<
        "INSERT INTO personnel_roles (personnel_id, project_id, role) ",
        "VALUES ($1, $2, $3) ",
        "ON CONFLICT (\"personnel_id\", \"project_id\") DO UPDATE SET role = EXCLUDED.role "
    >>,
    Params = [PersonnelId, ProjectId, Role],
    case db_query:insert(Query, Params) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec delete(PersonnelId, ProjectId) -> Result when
    PersonnelId :: non_neg_integer(),
    ProjectId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete(PersonnelId, ProjectId) ->
    Query = <<"DELETE FROM personnel_roles WHERE personnel_id = $1 AND project_id = $2">>,
    case db_query:delete(Query, [PersonnelId, ProjectId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec delete_for_account(PersonnelId) -> Result when
    PersonnelId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete_for_account(PersonnelId) ->
    Query = <<"DELETE FROM personnel_roles WHERE personnel_id = $1">>,
    case db_query:delete(Query, [PersonnelId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec delete_for_project(ProjectId) -> Result when
    ProjectId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete_for_project(ProjectId) ->
    Query = <<"DELETE FROM personnel_roles WHERE project_id = $1">>,
    case db_query:delete(Query, [ProjectId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

%% Local functions
