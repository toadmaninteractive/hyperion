-module(web_rest_callback_attachment).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include_lib("db/include/protocol_db.hrl").
-include("limits.hrl").
-include("session.hrl").
-include("protocol_data.hrl").
-include("protocol_test_case.hrl").
-include("protocol_notification.hrl").

%% Exported functions

-export([
    get_project_attachments/6,
    get_linked_attachments/2,
    get_attachment_links/1,
    delete_attachment_link/4
]).

%% API

-spec get_project_attachments(ProjectId, OrderBy, OrderDir, Offset, Limit, Needle) -> Result when
    ProjectId :: non_neg_integer(),
    OrderBy :: protocol_attachment:file_attachment_order_by(),
    OrderDir :: protocol_data:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Needle :: binary() | 'undefined',
    Result :: protocol_data:collection_slice(protocol_db:file_attachment()).

get_project_attachments(ProjectId, OrderBy, OrderDir, Offset, Limit, Needle) ->
    OrderBy1 = protocol_attachment:file_attachment_order_by_to_string(OrderBy),
    OrderDir1 = protocol_data:order_direction_to_string(OrderDir),
    {ok, Attachments} = db_if_file_attachments:get(ProjectId, OrderBy1, OrderDir1, Offset, ?slice_limit(Limit), Needle),
    {ok, Total} = db_if_file_attachments:get_count(ProjectId, Needle),
    #collection_slice{items = Attachments, total = Total}.

-spec get_attachment_links(Id :: non_neg_integer()) ->
    protocol_data:collection(protocol_db:file_attachment_link()).

get_attachment_links(Id) ->
    case db_if_file_attachment_links:get_linked(Id) of
        {ok, AttachmentLinks} -> #collection{items = AttachmentLinks};
        {error, Reason} -> web_rest_attachment_links:get_attachment_links_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec get_linked_attachments(Owner :: protocol_db:attachment_owner(), LinkedId :: non_neg_integer()) ->
    protocol_db:file_attachment().

get_linked_attachments(Owner, LinkedId) ->
    case db_if_file_attachments:get_linked(Owner, LinkedId) of
        {ok, Attachments} -> #collection{items = Attachments};
        {error, ?err_not_exists} -> web_rest_attachments:get_linked_attachments_404(#not_found_error{});
        {error, Reason} -> web_rest_attachments:get_linked_attachments_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

-spec delete_attachment_link(Id :: non_neg_integer(), Owner :: protocol_db:attachment_owner(), LinkedId :: non_neg_integer(), Req :: cowboy_req:req()) ->
    {protocol_data:generic_response(), cowboy_req:req()}.

delete_attachment_link(Id, Owner, LinkedId, #{?m_session := #session{user_id = UserId, username = Username}} = Req) ->
    case db_if_file_attachments:get_one(Id) of
        {ok, #file_attachment{filename = Filename, thumb_filename = ThumbFilename} = Attachment} ->
            {ok, AttachmentLink} = db_if_file_attachment_links:get_one(Id, Owner, LinkedId),
            case db_if_file_attachment_links:delete(Id, Owner, LinkedId) of
                ok ->
                    CanDelete = db_if_file_attachment_links:exists(Id) =:= {ok, false},
                    CanDeleteFiles = ?yesno(CanDelete, db_if_file_attachments:delete(Id) =:= ok, false),
                    ?doif(CanDeleteFiles, begin
                        delete_attachment_file(Filename),
                        delete_attachment_file(ThumbFilename)
                    end),
                    web_ws:broadcast(#attachment_unlinked{actor_id = UserId, actor_name = Username, attachment = Attachment, link = AttachmentLink}),
                    {#generic_response{result = true}, Req};
                {error, ?err_not_exists} ->
                    web_rest_attachment_unlink:delete_attachment_link_404(#not_found_error{});
                {error, Reason} ->
                    web_rest_attachment_unlink:delete_attachment_link_500(#internal_server_error{error = util_binary:to_binary(Reason)})
            end;
        {error, ?err_not_exists} ->
            web_rest_attachment_unlink:delete_attachment_link_404(#not_found_error{});
        {error, Reason} ->
            web_rest_attachment_unlink:delete_attachment_link_500(#internal_server_error{error = util_binary:to_binary(Reason)})
    end.

%% Local functions

delete_attachment_file(Filename) ->
    {ok, UploadDir} = web_config:upload_dir(),
    FilePath = filename:join([UploadDir, "attachments", Filename]),
    util_file:safe_delete(FilePath).
