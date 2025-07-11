-module(web_attachments).

-behaviour(gen_server).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include_lib("db/include/protocol_db.hrl").
-include("settings.hrl").
-include("kv.hrl").

%% Exported functions

-export([
    start_link/0
]).

%% gen_server callbacks

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    worker_pid :: pid() | 'undefined'
}).

-define(cleanup_interval, 60 * 60 * 1000). % 1 hour
-define(cleanup_expired_attachments, cleanup_expired_attachments).

%% API

-spec start_link() ->
    {'ok', pid()} | 'ignore' | {'error', {'already_started', pid()} | term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks
init(_Args) ->
    erlang:send_after(0, self(), ?cleanup_expired_attachments),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    logger:debug("unhandled call ~p from ~p~n", [Request, From], #{caption => ?MODULE}),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    logger:debug("unhandled cast ~p~n", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

handle_info(?cleanup_expired_attachments, State) ->
    Pid = proc_lib:spawn(fun cleanup_expired_attachments/0),
    erlang:monitor(process, Pid),
    {noreply, State#state{worker_pid = Pid}};

handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, #state{worker_pid = Pid} = State) ->
    erlang:send_after(?cleanup_interval, self(), ?cleanup_expired_attachments),
    {noreply, State#state{worker_pid = undefined}};

handle_info(Msg, State) ->
    logger:debug("unhandled info ~p~n", [Msg], #{caption => ?MODULE}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Local functions

cleanup_expired_attachments() ->
    {ok, UploadDir} = web_config:upload_dir(),
    AttachmentDir = filename:join([UploadDir, "attachments"]),
    {ok, Attachments} = db_if_file_attachments:get_expired(false),
    [begin
        % Delete original file if exists
        FilePath = filename:join([AttachmentDir, FileName]),
        ?doif(filelib:is_file(FilePath), file:delete(FilePath)),

        % Delete thumbnail file if exists
        ?doif(is_binary(ThumbFileName), begin
            ThumbFilePath = filename:join([AttachmentDir, ThumbFileName]),
            ?doif(filelib:is_file(ThumbFilePath), file:delete(ThumbFilePath))
        end),

        % Delete file attachment entry
        Result = db_if_file_attachments:delete(AttachmentId),
        ?doif(Result =:= ok, logger:info("Deleted expired file attachment <~B>, original filename: ~ts", [AttachmentId, OriginalFileName]))
    end || #file_attachment{id = AttachmentId, filename = FileName, thumb_filename = ThumbFileName, original_filename = OriginalFileName} <- Attachments].
