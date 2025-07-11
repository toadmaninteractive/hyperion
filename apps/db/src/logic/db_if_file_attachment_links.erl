-module(db_if_file_attachment_links).

%% Include files

-include_lib("epgsql/include/epgsql.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/3,
    get_linked/1,
    create/4,
    delete/3,
    delete_linked/1,
    exists/1,
    exists/3
]).

%% API

-spec get_one(AttachmentId, Owner, LinkedId) -> Result when
    AttachmentId :: non_neg_integer(),
    Owner :: protocol_db:attachment_owner(),
    LinkedId :: non_neg_integer(),
    Result :: {'ok', protocol_db:file_attachment_link()} | {'error', Reason :: atom()}.

get_one(AttachmentId, Owner, LinkedId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM file_attachment_links AS fal ",
        (common_joins())/binary,
        "WHERE fal.attachment_id = $1 AND fal.owner = $2 AND fal.linked_id = $3"
    >>,
    Params = [AttachmentId, protocol_db:attachment_owner_to_json(Owner), LinkedId],
    case db_query:select_one(Query, Params) of
        {ok, Item} -> {ok, protocol_db:file_attachment_link_from_json(Item)};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_linked(AttachmentId) -> Result when
    AttachmentId :: non_neg_integer(),
    Result :: {'ok', Items :: [protocol_db:file_attachment_link()]} | {'error', Reason :: atom()}.

get_linked(AttachmentId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM file_attachment_links AS fal ",
        (common_joins())/binary,
        "WHERE fal.attachment_id = $1 ",
        "ORDER BY created_at DESC"
    >>,
    case db_query:select(Query, [AttachmentId]) of
        {ok, Items} -> {ok, [protocol_db:file_attachment_link_from_json(Item) || Item <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec create(AttachmentId, Owner, LinkedId, PersonnelId) -> Result when
    AttachmentId :: non_neg_integer(),
    Owner :: protocol_db:attachment_owner(),
    LinkedId :: non_neg_integer(),
    PersonnelId :: non_neg_integer(),
    Result :: {'ok', AttachmentId :: non_neg_integer()} | {'error', Reason :: atom()}.

create(AttachmentId, Owner, LinkedId, PersonnelId) ->
    Query = <<
        "INSERT INTO file_attachment_links (attachment_id, owner, linked_id, personnel_id) ",
        "VALUES ($1, $2, $3, $4) ",
        "ON CONFLICT (attachment_id, owner, linked_id) DO NOTHING ",
        "RETURNING attachment_id::bigint"
    >>,
    Params = [AttachmentId, Owner, LinkedId, PersonnelId],
    case db_query:insert(Query, Params, [raw_error]) of
        {ok, 1, Columns, Rows} ->
            [#{<<"attachment_id">> := AttachmentId}] = db_util:result_to_json(Columns, Rows),
            {ok, AttachmentId};
        {error, #error{codename = check_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"file_attachment_links_linked_id_check">> -> {error, invalid_linked_id}
            end;
        {error, #error{codename = foreign_key_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"file_attachment_links_attachment_id_fkey">> -> {error, attachment_not_exists};
                <<"file_attachment_links_personnel_id_fkey">> -> {error, personnel_not_exists}
            end;
        {error, #error{codename = Code}} ->
            {error, Code}
    end.

-spec delete(AttachmentId, Owner, LinkedId) -> Result when
    AttachmentId :: non_neg_integer(),
    Owner :: protocol_db:attachment_owner(),
    LinkedId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete(AttachmentId, Owner, LinkedId) ->
    Query = <<
        "DELETE FROM file_attachment_links ",
        "WHERE attachment_id = $1 AND owner = $2 AND linked_id = $3"
    >>,
    case db_query:delete(Query, [AttachmentId, protocol_db:attachment_owner_to_json(Owner), LinkedId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec delete_linked(AttachmentId) -> Result when
    AttachmentId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete_linked(AttachmentId) ->
    Query = <<"DELETE FROM file_attachment_links WHERE attachment_id = $1">>,
    case db_query:delete(Query, [AttachmentId]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec exists(AttachmentId) -> Result when
    AttachmentId :: non_neg_integer(),
    Result :: {'ok', boolean()} | {'error', Reason :: atom()}.

exists(AttachmentId) ->
    Query = <<
        "SELECT COUNT(*)::bigint > 0 AS result ",
        "FROM file_attachment_links ",
        "WHERE attachment_id = $1"
    >>,
    case db_query:select_one(Query, [AttachmentId]) of
        {ok, #{<<"result">> := Result}} -> {ok, Result};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec exists(AttachmentId, Owner, LinkedId) -> Result when
    AttachmentId :: non_neg_integer(),
    Owner :: protocol_db:attachment_owner(),
    LinkedId :: non_neg_integer(),
    Result :: {'ok', boolean()} | {'error', Reason :: atom()}.

exists(AttachmentId, Owner, LinkedId) ->
    Query = <<
        "SELECT COUNT(*)::bigint > 0 AS result ",
        "FROM file_attachment_links ",
        "WHERE attachment_id = $1 AND owner = $2 AND linked_id = $3"
    >>,
    case db_query:select_one(Query, [AttachmentId, protocol_db:attachment_owner_to_json(Owner), LinkedId]) of
        {ok, #{<<"result">> := Result}} -> {ok, Result};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % fal : file_attachment_links
    % per : personnel
    % ss  : setup_steps
    % tc  : test_cases
    % tr1 : test_runs
    % tr2 : test_runs
    % tri : test_run_items
    <<
        "fal.attachment_id AS attachment_id, ",
        "fal.owner AS owner, ",
        "fal.linked_id AS linked_id, ",
        "(CASE ",
            "WHEN fal.owner = 'setup_step' THEN ss.project_id ",
            "WHEN fal.owner = 'test_case' THEN tc.project_id ",
            "WHEN fal.owner = 'test_run' THEN tr1.project_id ",
            "WHEN fal.owner = 'test_run_item' THEN tr2.project_id ",
        "END) AS project_id, "
        "fal.personnel_id AS personnel_id, ",
        "per.username AS personnel_name, ",
        "fal.created_at AS created_at "
    >>.

common_joins() ->
    <<
        "LEFT OUTER JOIN personnel AS per ON (per.id = fal.personnel_id) ",
        "LEFT OUTER JOIN setup_steps AS ss ON (ss.id = fal.linked_id AND fal.owner = 'setup_step') ",
        "LEFT OUTER JOIN test_cases AS tc ON (tc.id = fal.linked_id AND fal.owner = 'test_case') ",
        "LEFT OUTER JOIN test_runs AS tr1 ON (tr1.id = fal.linked_id AND fal.owner = 'test_run') ",
        "LEFT OUTER JOIN test_run_items AS tri ON (tri.id = fal.linked_id AND fal.owner = 'test_run_item') ",
        "LEFT OUTER JOIN test_runs AS tr2 ON (tr2.id = tri.run_id AND fal.owner = 'test_run_item') "
    >>.
