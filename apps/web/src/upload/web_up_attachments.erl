-module(web_up_attachments).

-behaviour(cowboy_handler).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include_lib("db/include/protocol_db.hrl").
-include("protocol_notification.hrl").
-include("http.hrl").
-include("session.hrl").
-include("upload.hrl").

%% Exported functions

-export([
    init/2
]).

-define(thumb_width, 128).
-define(thumb_height, 128).

%% API

init(#{?m_session := #session{key = {personnel, _}, user_id = UserId, username = Username}} = Req, #upload_config{max_file_size = MaxFileSize, target_dir = AttachmentDir} = Opts) ->
    Owner = protocol_db:attachment_owner_from_json(cowboy_req:binding(owner, Req)),
    LinkedId = binary_to_integer(cowboy_req:binding(linked_id, Req)),
    Req1 = case cowboy_req:read_part(Req, #{length => MaxFileSize}) of
        {ok, Headers, R1} ->
            case cow_multipart:form_data(Headers) of
                {data, _FieldName} ->
                    web_util:reply_json_explicit(?http_bad_request, #{error => not_a_file}, R1);
                {file, <<"attachment">>, FileName, ContentType} ->
                    case cowboy_req:read_part_body(R1, #{length => MaxFileSize, period => 7200}) of
                        {ok, FileData, R2} ->
                            case attachment_owner_exists(Owner, LinkedId) of
                                true ->
                                    case add_attachment(FileData, FileName, ContentType, AttachmentDir, Owner, LinkedId, UserId, Username) of
                                        {ok, StoredFileName, ThumbFileName} ->
                                            web_util:reply_json_explicit(?http_ok, #{filename => StoredFileName, thumb_filename => ThumbFileName}, R2);
                                        {error, Reason} ->
                                            web_util:reply_json_explicit(?http_bad_request, #{error => Reason}, R2)
                                    end;
                                false ->
                                    web_util:reply_json_explicit(?http_bad_request, #{error => owner_not_exists}, R2)
                            end;
                        {more, _, R2} ->
                            web_util:reply_json_explicit(?http_bad_request, #{error => invalid_file_length}, R2)
                    end;
                {file, _, _, _} ->
                    web_util:reply_json_explicit(?http_bad_request, #{error => invalid_name}, R1)
            end;
        {error, Reason} ->
            web_util:reply_json_explicit(?http_internal_server_error, #{error => Reason}, Req)
    end,
    {ok, Req1, Opts}.

%% Local functions

add_attachment(FileData, OriginalFileName, ContentType, Dir, Owner,LinkedId, UserId, Username) ->
    % Get file size and SHA hash
    Size = byte_size(FileData),
    ShaHex = util_hex:from_binary(crypto:hash(sha, FileData)),

    % Check if attachment already exists (by hash and file size)
    case db_if_file_attachments:get_one_by_size_sha(Size, ShaHex) of
        {ok, #file_attachment{id = AttachmentId, filename = FileName, thumb_filename = ThumbFileName}} ->
            db_if_file_attachment_links:create(AttachmentId, Owner, LinkedId, UserId),
            {ok, Attachment} = db_if_file_attachments:get_one(AttachmentId),
            {ok, AttachmentLink} = db_if_file_attachment_links:get_one(AttachmentId, Owner, LinkedId),
            web_ws:broadcast(#attachment_linked{actor_id = UserId, actor_name = Username, attachment = Attachment, link = AttachmentLink}),
            {ok, FileName, ThumbFileName};
        {error, ?err_not_exists} ->
            % Construct filenames
            Ext = filename:extension(OriginalFileName),
            BaseFileName = <<(integer_to_binary(Size))/binary, "_", ShaHex/binary>>,
            StoredFileName = <<BaseFileName/binary, Ext/binary>>,
            StoredFilePath = filename:join([Dir, StoredFileName]),
            ThumbFileName = <<BaseFileName/binary, "_thumb", Ext/binary>>,
            ThumbFilePath = filename:join([Dir, ThumbFileName]),

            try
                % Save to file
                filelib:ensure_dir(StoredFilePath),
                file:write_file(StoredFilePath, FileData),

                % Create attachment and link it
                HasThumb = mk_thumbnail(StoredFilePath, ThumbFilePath) =:= ok,
                {ok, AttachmentId} = db_if_file_attachments:create(Size, ShaHex, StoredFileName, ?yesno(HasThumb, ThumbFileName, undefined), OriginalFileName, ContentType, UserId),
                db_if_file_attachment_links:create(AttachmentId, Owner, LinkedId, UserId),

                % Notify others
                {ok, Attachment} = db_if_file_attachments:get_one(AttachmentId),
                {ok, AttachmentLink} = db_if_file_attachment_links:get_one(AttachmentId, Owner, LinkedId),
                web_ws:broadcast(#attachment_linked{actor_id = UserId, actor_name = Username, attachment = Attachment, link = AttachmentLink}),

                {ok, StoredFileName, ?yesno(HasThumb, ThumbFileName, ?null)}
            catch Type:What:StackTrace ->
                ?doif(filelib:is_regular(StoredFilePath), file:delete(StoredFilePath)),
                ?doif(filelib:is_regular(ThumbFilePath), file:delete(ThumbFilePath)),
                logger:error("Failed to upload file attachment <~ts> (reason: ~p:~p)", [OriginalFileName, Type, What], #{caption => ?MODULE, stacktrace => StackTrace}),
                {error, What}
            end
    end.

mk_thumbnail(FilePath, ThumbPath) ->
    case gm:identify_explicit(FilePath, [width, height]) of
        % Need to crop and resize
        {ok, #{width := Width, height := Height}} ->
            MinDimension = min(Width, Height),
            InCommands = [auto_orient],
            OutCommands = [
                auto_orient,
                {gravity, "Center"},
                {crop, MinDimension, MinDimension, 0, 0},
                {resize, ?thumb_width, ?thumb_height},
                strip,
                {'+profile', "*"}
            ],

            case gm:convert(FilePath, ThumbPath, InCommands, OutCommands) of
                ok -> ok;
                {error, Reason} -> {error, Reason}
            end;

        % Not an image
        _ -> {error, invalid_image}
    end.

attachment_owner_exists(setup_step, SetupId) ->
    {ok, Exists} = db_if_setup_steps:exists(SetupId),
    Exists;
attachment_owner_exists(test_case, CaseId) ->
    {ok, Exists} = db_if_test_cases:exists(CaseId),
    Exists;
attachment_owner_exists(test_run, RunId) ->
    {ok, Exists} = db_if_test_runs:exists(RunId),
    Exists;
attachment_owner_exists(test_run_item, ItemId) ->
    {ok, Exists} = db_if_test_run_items:exists(ItemId),
    Exists.
