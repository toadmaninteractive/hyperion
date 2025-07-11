-module(db_if_personnel_group_membership).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_all/0,
    create/2,
    create_explicit/2,
    delete/2,
    delete_explicit/2,
    is_member/2,
    is_member_explicit/2
]).

%% API

-spec get_all() ->
    {'ok', Map :: maps:map(GroupName :: binary(), [Username :: binary(), ...])} | {'error', Reason :: atom()}.

get_all() ->
    Statement = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM personnel_group_membership AS pergm ",
        (common_joins())/binary,
        "ORDER BY group_name ASC, username DESC"
    >>,
    case db_query:select(Statement, []) of
        {ok, Items} ->
            Result = lists:foldl(fun(#{<<"username">> := Username, <<"group_name">> := GroupName}, AccMap) ->
                Members = maps:get(GroupName, AccMap, ordsets:new()),
                AccMap#{GroupName => ordsets:add_element(Username, Members)}
            end, #{}, Items),
            {ok, Result};
        {error, Reason} ->
            {error, Reason}
    end.

-spec create(PersonnelId, GroupId) -> Result when
    PersonnelId :: non_neg_integer(),
    GroupId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

create(PersonnelId, GroupId) ->
    Query = <<"INSERT INTO personnel_group_membership (personnel_id, group_id) VALUES ($1, $2)">>,
    case db_query:insert(Query, [PersonnelId, GroupId]) of
        {ok, 1} -> ok;
        {error, unique_violation} -> {error, ?err_already_exists};
        {error, Other} -> {error, Other}
    end.

-spec create_explicit(Username, GroupName) -> Result when
    Username :: binary(),
    GroupName :: binary(),
    Result :: 'ok' | {'error', Reason :: atom()}.

create_explicit(Username, GroupName) ->
    CredentialResult = db_if_personnel:credentials(Username),
    GroupResult = db_if_personnel_groups:get_one_by_name(GroupName),
    case {CredentialResult, GroupResult} of
        {{ok, PersonnelId, _, _, _}, {ok, #{<<"id">> := GroupId}}} -> create(PersonnelId, GroupId);
        _ -> {error, failure}
    end.

-spec delete(PersonnelId, GroupId) -> Result when
    PersonnelId :: non_neg_integer(),
    GroupId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete(PersonnelId, GroupId) ->
    Query = <<"DELETE FROM personnel_group_membership WHERE personnel_id = $1 AND group_id = $2">>,
    case db_query:delete(Query, [PersonnelId, GroupId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec delete_explicit(Username, GroupName) -> Result when
    Username :: binary(),
    GroupName :: binary(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete_explicit(Username, GroupName) ->
    CredentialResult = db_if_personnel:credentials(Username),
    GroupResult = db_if_personnel_groups:get_one_by_name(GroupName),
    case {CredentialResult, GroupResult} of
        {{ok, PersonnelId, _, _, _}, {ok, #{<<"id">> := GroupId}}} -> delete(PersonnelId, GroupId);
        _ -> {error, failure}
    end.

-spec is_member(PersonnelId, GroupId) -> Result when
    PersonnelId :: non_neg_integer(),
    GroupId :: non_neg_integer(),
    Result :: {'ok', boolean()} | {'error', Reason :: atom()}.

is_member(PersonnelId, GroupId) ->
    Query = <<
        "SELECT COUNT(id)::integer > 0 AS is_member ",
        "FROM personnel_group_membership ",
        "WHERE personnel_id = $1 AND group_id = $2"
    >>,
    case db_query:select_one(Query, [PersonnelId, GroupId]) of
        {ok, #{<<"is_member">> := IsMember}} -> {ok, IsMember};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec is_member_explicit(Username, GroupName) -> Result when
    Username :: binary(),
    GroupName :: binary(),
    Result :: {'ok', boolean()} | {'error', Reason :: atom()}.

is_member_explicit(Username, GroupName) ->
    CredentialResult = db_if_personnel:credentials(Username),
    GroupResult = db_if_personnel_groups:get_one_by_name(GroupName),
    case {CredentialResult, GroupResult} of
        {{ok, PersonnelId, _, _, _}, {ok, #{<<"id">> := GroupId}}} -> is_member(PersonnelId, GroupId);
        _ -> {error, failure}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % per   : personnel
    % perg  : personnel_groups
    % pergm : personnel_group_membership
    <<
        "pergm.personnel_id AS personnel_id, ",
        "per.username AS username, ",
        "pergm.group_id AS group_id, ",
        "perg.name AS group_name "
    >>.

common_joins() ->
    <<
        "LEFT OUTER JOIN personnel AS per ON (per.id = pergm.personnel_id) ",
        "LEFT OUTER JOIN personnel_groups AS perg ON (perg.id = pergm.group_id) "
    >>.
