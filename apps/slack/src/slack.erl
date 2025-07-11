-module(slack).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol_db.hrl").
-include("slack_protocol.hrl").

%% Exported functions

-export([
    test_run_started/5,
    test_run_completed/7
]).

%% API

-spec test_run_started(ReceiverId, ProjectKey, TestRunId, TestRunTitle, NumTotal) -> Result when
    ReceiverId :: binary(),
    ProjectKey :: binary(),
    TestRunId :: non_neg_integer(),
    TestRunTitle :: binary() | 'undefined',
    NumTotal :: non_neg_integer(),
    Result :: pid().

test_run_started(ReceiverId, ProjectKey, TestRunId, TestRunTitle, NumTotal) ->
    IsValidTitle = is_binary(TestRunTitle) andalso TestRunTitle =/= <<>>,
    Title = ?yesno(IsValidTitle, <<"\"", TestRunTitle/binary, "\"">>, <<"#", (integer_to_binary(TestRunId))/binary>>),
    Message = <<"<", (testing_url(ProjectKey, TestRunId))/binary, "|*Hyperion: test run ", Title/binary, " started*>"/utf8>>,
    Pretext = <<"*", (integer_to_binary(NumTotal))/binary, "* test cases to go">>,
    Attachments = [
        #attachment_block{
            pretext = Pretext,
            color = in_progress_color(),
            footer = bot_title(),
            footer_icon = bot_icon_url(),
            ts = time:seconds()
        }
    ],
    Request = #post_message_ex_request{
        channel = ReceiverId,
        text = Message,
        mrkdwn = true,
        attachments = Attachments
    },
    send_async(fun slack_api:post_message_ex/1, [Request]).

-spec test_run_completed(ReceiverId, ProjectKey, TestRunId, TestRunTitle, NumSuccess, NumBlocked, NumFailed) -> Result when
    ReceiverId :: binary(),
    ProjectKey :: binary(),
    TestRunId :: non_neg_integer(),
    TestRunTitle :: binary() | 'undefined',
    NumSuccess :: non_neg_integer(),
    NumBlocked :: non_neg_integer(),
    NumFailed :: non_neg_integer(),
    Result :: pid().

test_run_completed(ReceiverId, ProjectKey, TestRunId, TestRunTitle, NumSuccess, NumBlocked, NumFailed) ->
    IsValidTitle = is_binary(TestRunTitle) andalso TestRunTitle =/= <<>>,
    Title = ?yesno(IsValidTitle, <<"\"", TestRunTitle/binary, "\"">>, <<"#", (integer_to_binary(TestRunId))/binary>>),
    % Message = <<"<", (manage_url(ProjectKey, TestRunId))/binary, "|*Hyperion: test run ", Title/binary, " completed*>"/utf8>>,
    Message = <<"<", (testing_url(ProjectKey, TestRunId))/binary, "|*Hyperion: test run ", Title/binary, " completed*>"/utf8>>,
    NumTotal = NumSuccess + NumBlocked + NumFailed,
    Pretext = <<"*", (integer_to_binary(NumTotal))/binary, "* test cases processed">>,
    Attachments = [
        #attachment_block{pretext = Pretext},
        result_item(<<"Success">>, NumSuccess, NumTotal, success_color()),
        result_item(<<"Blocked">>, NumBlocked, NumTotal, blocked_color()),
        result_item(<<"Failed">>, NumFailed, NumTotal, failed_color()),
        #attachment_block{footer = bot_title(), footer_icon = bot_icon_url(), ts = time:seconds()}
    ],
    Request = #post_message_ex_request{
        channel = ReceiverId,
        text = Message,
        mrkdwn = true,
        attachments = Attachments
    },
    send_async(fun slack_api:post_message_ex/1, [Request]).

%% Local functions

send_async(Fun, Args) ->
    proc_lib:spawn(fun() ->
        try erlang:apply(Fun, Args)
        catch _:_:_ -> ignore
        end
    end).

bot_title() -> <<"Hyperion Slack Bot">>.
bot_icon_url() -> <<"https://hyperion.yourcompany.com/assets/app/media/img/logos/hyperion/hyperion_i32.png">>.

in_progress_color() -> <<"#1d9bd1">>.
success_color() -> <<"#34bfa3">>.
blocked_color() -> <<"#ffd700">>.
failed_color() -> <<"#e7828c">>.

result_item(Title, Count, Total, Color) ->
    FullTitle = <<Title/binary, ": ", (integer_to_binary(Count))/binary, " / ", (integer_to_binary(Total))/binary>>,
    Percent = round(100 * Count / Total),
    #attachment_block{
        fallback = FullTitle,
        color = Color,
        title = <<FullTitle/binary, " (", (integer_to_binary(Percent))/binary,"%)">>
    }.

testing_url(ProjectKey, TestRunId) ->
    {ok, RootUrl} = web_config:url(),
    <<RootUrl/binary, "/testing/", ProjectKey/binary, "/", (integer_to_binary(TestRunId))/binary, "/details">>.

%user_id(undefined) -> undefined;
%user_id(Name) ->
%    NameV2 = binary:replace(Name, <<".">>, <<" ">>, [global]),
%    case slack_bot:user_id(Name) of
%        undefined -> slack_bot:user_id(NameV2);
%        UID -> UID
%    end.

%user_link(undefined) -> undefined;
%user_link(Id) -> <<"slack://user?team=", (slack_config:team_id())/binary, "&id=", Id/binary>>.
