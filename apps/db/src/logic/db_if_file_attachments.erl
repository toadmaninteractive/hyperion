-module(db_if_file_attachments).

%% Include files

-include_lib("epgsql/include/epgsql.hrl").
-include_lib("aplib/include/apmacros.hrl").
-include("protocol.hrl").

%% Exported functions

-export([
    get_one/1,
    get_one_by_size_sha/2,
    get/6,
    get_count/2,
    get_linked/2,
    get_expired/1,
    create/7,
    delete/1,
    exists/1
]).

%% API

-spec get_one(AttachmentId) -> Result when
    AttachmentId :: non_neg_integer(),
    Result :: {'ok', protocol_db:file_attachment()} | {'error', Reason :: atom()}.

get_one(AttachmentId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM file_attachments AS fa ",
        (common_joins())/binary,
        "WHERE fa.id = $1"
    >>,
    case db_query:select_one(Query, [AttachmentId]) of
        {ok, Item} -> {ok, protocol_db:file_attachment_from_json(Item)};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_one_by_size_sha(FileSize, FileSha) -> Result when
    FileSize :: non_neg_integer(),
    FileSha :: binary(),
    Result :: {'ok', protocol_db:file_attachment()} | {'error', Reason :: atom()}.

get_one_by_size_sha(FileSize, FileSha) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM file_attachments AS fa ",
        (common_joins())/binary,
        "WHERE fa.file_size = $1 AND fa.file_sha = TRIM(LOWER($2))"
    >>,
    case db_query:select_one(Query, [FileSize, FileSha]) of
        {ok, Item} -> {ok, protocol_db:file_attachment_from_json(Item)};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get(ProjectId, OrderBy, OrderDir, Offset, Limit, Needle) -> Result when
    ProjectId :: non_neg_integer(),
    OrderBy :: binary(),
    OrderDir :: binary(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Needle :: binary() | 'undefined',
    Result :: {'ok', [protocol_db:file_attachment()]} | {'error', Reason :: atom()}.

get(ProjectId, OrderBy, OrderDir, Offset, Limit, Needle) ->
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<" AND (strpos(LOWER(fa.original_filename), LOWER($2)) > 0 OR strpos(LOWER(fa.content_type), LOWER($2)) > 0) ">>, <<>>),
    Filter = db_util:mk_query_filter(OrderBy, OrderDir, Offset, Limit),
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM file_attachments AS fa ",
        (common_joins())/binary,
        "WHERE fa.id IN (SELECT * FROM get_project_file_attachment_ids($1)) ",
        NeedlePart/binary,
        Filter/binary
    >>,
    Params = [ProjectId] ++ ?yesno(HasNeedle, [Needle], []),
    case db_query:select(Query, Params) of
        {ok, Items} -> {ok, [protocol_db:file_attachment_from_json(Item) || Item <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec get_count(ProjectId, Needle) -> Result when
    ProjectId :: non_neg_integer(),
    Needle :: binary() | 'undefined',
    Result :: {'ok', Count :: non_neg_integer()} | {'error', Reason :: atom()}.

get_count(ProjectId, Needle) ->
    HasNeedle = is_binary(Needle) andalso Needle =/= <<>>,
    NeedlePart = ?yesno(HasNeedle, <<" AND (strpos(LOWER(fa.original_filename), LOWER($2)) > 0 OR strpos(LOWER(fa.content_type), LOWER($2)) > 0) ">>, <<>>),
    Query = <<
        "SELECT COUNT(*)::bigint AS count ",
        "FROM file_attachments AS fa ",
        "WHERE fa.id IN (SELECT * FROM get_project_file_attachment_ids($1)) ",
        NeedlePart/binary
    >>,
    Params = [ProjectId] ++ ?yesno(HasNeedle, [Needle], []),
    case db_query:select_one(Query, Params) of
        {ok, #{<<"count">> := Count}} -> {ok, Count};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

-spec get_linked(Owner :: protocol_db:attachment_owner(), LinkedId :: binary()) ->
    {'ok', Items :: [protocol_db:file_attachment()]} | {'error', Reason :: atom()}.

get_linked(Owner, LinkedId) ->
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM file_attachments AS fa ",
        (common_joins())/binary,
        "LEFT OUTER JOIN file_attachment_links AS fal ON (fal.attachment_id = fa.id) "
        "WHERE fa.id IS NOT NULL AND fal.owner = $1 AND fal.linked_id = $2 ",
        "ORDER BY fal.created_at ASC"
    >>,
    case db_query:select(Query, [protocol_db:attachment_owner_to_json(Owner), LinkedId]) of
        {ok, Items} -> {ok, [protocol_db:file_attachment_from_json(Item) || Item <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec get_expired(Instant :: boolean()) ->
    {'ok', Items :: [protocol_db:file_attachment()]} | {'error', Reason :: atom()}.

get_expired(Instant) ->
    InstantQuery = ?yesno(Instant, <<>>, <<" AND (EXTRACT(EPOCH FROM current_timestamp - fa.created_at) / 3600) > 1 ">>),
    Query = <<
        "SELECT ", (common_entity_fields())/binary,
        "FROM file_attachments AS fa ",
        (common_joins())/binary,
        "LEFT OUTER JOIN file_attachment_links AS fal ON (fal.attachment_id = fa.id) ",
        "WHERE fal.attachment_id IS NULL ", InstantQuery/binary
    >>,
    case db_query:select(Query, []) of
        {ok, Items} -> {ok, [protocol_db:file_attachment_from_json(Item) || Item <- Items]};
        {error, Reason} -> {error, Reason}
    end.

-spec create(FileSize, FileSha, FileName, ThumbFileName, OriginalFileName, ContentType, PersonnelId) -> Result when
    FileSize :: non_neg_integer(),
    FileSha :: binary(),
    FileName :: binary(),
    ThumbFileName :: binary() | 'undefined',
    OriginalFileName :: binary() | 'undefined',
    ContentType :: binary() | 'undefined',
    PersonnelId :: non_neg_integer(),
    Result :: {'ok', AttachmentId :: non_neg_integer()} | {'error', Reason :: atom()}.

create(FileSize, FileSha, FileName, ThumbFileName, OriginalFileName, ContentType, PersonnelId) ->
    Query = <<
        "INSERT INTO file_attachments (file_size, file_sha, filename, thumb_filename, original_filename, content_type, personnel_id) ",
        "VALUES ($1, TRIM(LOWER($2)), TRIM($3), TRIM($4), TRIM($5), TRIM($6), $7) ",
        "RETURNING id::bigint"
    >>,
    Params = [FileSize, FileSha, FileName, ThumbFileName, OriginalFileName, ContentType, PersonnelId],
    case db_query:insert(Query, Params, [raw_error]) of
        {ok, 1, Columns, Rows} ->
            [#{<<"id">> := AttachmentId}] = db_util:result_to_json(Columns, Rows),
            {ok, AttachmentId};
        {error, #error{codename = check_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"file_attachments_file_size_check">> -> {error, invalid_file_size};
                <<"file_attachments_file_sha_check">> -> {error, invalid_file_sha};
                <<"file_attachments_filename_check">> -> {error, invalid_filename}
            end;
        {error, #error{codename = unique_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"fa_file_hash_index">> -> {error, attachment_already_exists}
            end;
        {error, #error{codename = foreign_key_violation, extra = ExtraProps}} ->
            case proplists:get_value(constraint_name, ExtraProps, undefined) of
                <<"file_attachments_personnel_id_fkey">> -> {error, personnel_not_exists}
            end;
        {error, #error{codename = Code}} ->
            {error, Code}
    end.

-spec delete(AttachmentId) -> Result when
    AttachmentId :: non_neg_integer(),
    Result :: 'ok' | {'error', Reason :: atom()}.

delete(AttachmentId) ->
    Query = <<"DELETE FROM file_attachments WHERE id = $1">>,
    case db_query:delete(Query, [AttachmentId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, ?err_not_exists};
        {error, Reason} -> {error, Reason}
    end.

-spec exists(AttachmentId) -> Result when
    AttachmentId :: non_neg_integer(),
    Result :: {'ok', boolean()} | {'error', Reason :: atom()}.

exists(AttachmentId) ->
    Query = <<"SELECT COUNT(*)::bigint > 0 AS result FROM file_attachments WHERE id = $1">>,
    case db_query:select_one(Query, [AttachmentId]) of
        {ok, #{<<"result">> := Result}} -> {ok, Result};
        {error, Reason} -> {error, Reason};
        undefined -> {error, ?err_not_exists}
    end.

%% Local functions

common_entity_fields() ->
    % Aliases:
    % fa  : file_attachments
    % per : personnel
    <<
        "fa.id AS id, ",
        "fa.file_size AS file_size, ",
        "fa.file_sha AS file_sha, ",
        "fa.filename AS filename, ",
        "fa.thumb_filename AS thumb_filename, ",
        "fa.original_filename AS original_filename, ",
        "fa.content_type AS content_type, ",
        "fa.personnel_id AS personnel_id, ",
        "per.username AS personnel_name, ",
        "fa.created_at AS created_at "
    >>.

common_joins() ->
    <<
        "LEFT OUTER JOIN personnel AS per ON (per.id = fa.personnel_id) "
    >>.
