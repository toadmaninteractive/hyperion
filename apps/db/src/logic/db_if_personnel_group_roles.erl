-module(db_if_personnel_group_roles).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/2,
    get_for_group/6,
    get_for_group_count/2,
    get_for_project/6,
    get_for_project_count/2,
    set/3,
    delete/2,
    delete_for_group/1,
    delete_for_project/1
]).

%% API

-spec get_one(GroupId, ProjectId) -> Result when
    GroupId :: non_neg_integer(),
    ProjectId :: non_neg_integer(),
    Result :: {'ok', binary()} | {'error', Reason :: atom()}.

get_one(GroupId, ProjectId) ->
    Query = <<"SELECT role FROM personnel_group_roles WHERE group_id = $1 AND project_id = $2">>,
    case db_query:select_one(Query, [GroupId, ProjectId]) of
        {ok, #{<<"role">> := Role}} -> {ok, Role};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_for_group(GroupId, Needle, OrderBy, OrderDir, Offset, Limit) -> Result when
    GroupId :: binary(),
    Needle :: binary() | 'undefined',
    OrderBy :: binary(),
    OrderDir :: binary(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Result :: {'ok', Items :: [jsx:json_term()]} | {'error', Reason :: atom()}.

get_for_group(GroupId, Needle, OrderBy, OrderDir, Offset, Limit) ->
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<" WHERE (strpos(LOWER(proj.id), LOWER($2)) > 0 OR strpos(LOWER(proj.title), LOWER($2)) > 0) ">>, <<>>),
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    Query = <<
        "SELECT ",
            "perg.id AS group_id, ",
            "perg.name AS group_name, ",
            "proj.id AS project_id, ",
            "proj.title AS project_title, ",
            "pergr.role AS group_role ",
        "FROM projects AS proj ",
        "LEFT OUTER JOIN personnel_groups AS perg ON (perg.id = $1) ",
        "LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.project_id = proj.id AND pergr.group_id = $1) ",
        NeedlePart/binary,
        Filter/binary
    >>,
    Params = [GroupId] ++ ?yesno(HasNeedle, [Needle], []),
    db_query:select(Query, Params).

-spec get_for_group_count(GroupId, Needle) -> Result when
    GroupId :: binary(),
    Needle :: binary() | 'undefined',
    Result :: {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_for_group_count(GroupId, Needle) ->
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<" WHERE (strpos(LOWER(proj.id), LOWER($2)) > 0 OR strpos(LOWER(proj.title), LOWER($2)) > 0)">>, <<>>),
    Query = <<
        "SELECT COUNT(*)::bigint AS count ",
        "FROM projects AS proj ",
        "LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.project_id = proj.id AND pergr.group_id = $1) ",
        NeedlePart/binary
    >>,
    Params = [GroupId] ++ ?yesno(HasNeedle, [Needle], []),
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
    NeedlePart = ?yesno(HasNeedle, <<" WHERE strpos(LOWER(perg.name), LOWER($2)) > 0 ">>, <<>>),
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    Query = <<
        "SELECT ",
            "perg.id AS group_id, ",
            "perg.name AS group_name, ",
            "proj.id AS project_id, ",
            "proj.title AS project_title, ",
            "pergr.role AS group_role ",
        "FROM personnel_groups AS perg ",
        "LEFT OUTER JOIN projects AS proj ON (proj.id = $1) ",
        "LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.group_id = perg.id AND pergr.project_id = $1) ",
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
    NeedlePart = ?yesno(HasNeedle, <<" WHERE strpos(LOWER(perg.name), LOWER($2)) > 0">>, <<>>),
    Query = <<
        "SELECT COUNT(*)::bigint AS count ",
        "FROM personnel_groups AS perg ",
        "LEFT OUTER JOIN personnel_group_roles AS pergr ON (pergr.group_id = perg.id AND pergr.project_id = $1) ",
        NeedlePart/binary
    >>,
    Params = [ProjectId] ++ ?yesno(HasNeedle, [Needle], []),
    case db_query:select_one(Query, Params) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec set(GroupId, ProjectId, Role) -> Result when
    GroupId :: non_neg_integer(),
    ProjectId :: non_neg_integer(),
    Role :: atom(),
    Result :: 'ok' |  {'error', Reason :: atom()}.

set(GroupId, ProjectId, Role) ->
    Query = <<
        "INSERT INTO personnel_group_roles (group_id, project_id, role) ",
        "VALUES ($1, $2, $3) ",
        "ON CONFLICT (\"group_id\", \"project_id\") DO UPDATE SET role = EXCLUDED.role "
    >>,
    Params = [GroupId, ProjectId, Role],
    case db_query:insert(Query, Params) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec delete(GroupId, ProjectId) -> Result when
    GroupId :: non_neg_integer(),
    ProjectId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete(GroupId, ProjectId) ->
    Query = <<"DELETE FROM personnel_group_roles WHERE group_id = $1 AND project_id = $2">>,
    case db_query:delete(Query, [GroupId, ProjectId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec delete_for_group(GroupId) -> Result when
    GroupId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete_for_group(GroupId) ->
    Query = <<"DELETE FROM personnel_group_roles WHERE group_id = $1">>,
    case db_query:delete(Query, [GroupId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec delete_for_project(ProjectId) -> Result when
    ProjectId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete_for_project(ProjectId) ->
    Query = <<"DELETE FROM personnel_group_roles WHERE project_id = $1">>,
    case db_query:delete(Query, [ProjectId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

%% Local functions
