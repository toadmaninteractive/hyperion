-module(web_rest_callback_personnel).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include_lib("db/include/protocol_common.hrl").
-include_lib("db/include/protocol_db.hrl").
-include("session.hrl").
-include("protocol_data.hrl").
-include("protocol_auth.hrl").

%% Exported functions

-export([
    get_personnel_status/1,
    login_personnel/2,
    logout_personnel/2,
    get_my_personnel_profile/1
]).

%% API

-spec get_personnel_status(Req) -> Response when
    Req :: cowboy_req:req(),
    Response :: {protocol_personnel:personnel_status_response(), cowboy_req:req()}.

get_personnel_status(#{?m_session := #session{key = {?actor_personnel, _SessionId}, user_id = UserId}} = Req) ->
    {ok, #{<<"email">> := Email, <<"username">> := Username}} = db_if_personnel:get_one(UserId),
    {#personnel_status_response{
        logged_in = true,
        email = web_util:maybe_null(Email),
        username = web_util:maybe_null(Username),
        user_id = UserId
    }, Req};
get_personnel_status(Req) ->
    {#personnel_status_response{logged_in = false}, Req}.

-spec login_personnel(Request, Req) -> Response when
    Request :: protocol_personnel:personnel_login_request(),
    Req :: cowboy_req:req(),
    Response :: {protocol_personnel:personnel_login_response(), cowboy_req:req()}.

login_personnel(_Request, #{?m_session := #session{}} = Req) ->
    {#personnel_login_response{result = false, error = already_logged_in}, Req};
login_personnel(#personnel_login_request{username = ReqUsername, password = ReqPassword}, Req) ->
    try
        LdapAuthResult = access:authenticate(ReqUsername, ReqPassword),
        LocalCredentials = db_if_personnel:credentials(ReqUsername),
        ?doif(LdapAuthResult =/= ok, begin
            {Type, What} = LdapAuthResult,
            logger:debug("Personnel authentication ~p (username: ~s, reason: ~p)~n", [Type, ReqUsername, What], #{caption => ?MODULE})
        end),
        LoginResult = case {LdapAuthResult, LocalCredentials} of
            % Account exists in LDAP but absent in local DB: create new personnel account
            {ok, {error, ?err_not_exists}} ->
                {ok, PersonnelId} = db_if_personnel:create(ReqUsername, ReqUsername, ?null, ?null),
                {ok, PersonnelId, ?null};

            % Account exists both in LDAP and in local DB, but is blocked
            {ok, {ok, _, _, true = _IsBlocked, _}} ->
                {error, account_is_blocked};

            % Account exists both in LDAP and in local DB, but is deleted: undelete account
            {ok, {ok, PersonnelId, PersonnelEmail, _, true = _IsDeleted}} ->
                db_if_personnel:undelete(PersonnelId),
                {ok, PersonnelId, PersonnelEmail};

            % Account exists both in LDAP and in local DB
            {ok, {ok, PersonnelId, PersonnelEmail, _, _}} ->
                {ok, PersonnelId, PersonnelEmail};

            % Account does not exist in LDAP, but exists in local DB: delete account and all of its sessions
            {{reject, unknown_user}, {ok, PersonnelId, _, _}} ->
                web_session:delete_for(?actor_personnel, PersonnelId),
                db_if_personnel:delete(PersonnelId),
                {error, account_is_deleted};

            % Account does not exist both in LDAP and in local DB
            {{reject, unknown_user}, _} ->
                {error, account_not_exists};

            % Invalid username / password pair
            {{reject, invalidCredentials}, _} ->
                {error, invalid_password}
        end,

        case LoginResult of
            {ok, UserId, Email} ->
                {ok, SessionId} = web_session:create(?actor_personnel, UserId),
                {ok, EncodedSession} = web_session:encode(?actor_personnel, SessionId),
                Req1 = cowboy_req:set_resp_header(?x_session_id_cs, EncodedSession, Req),
                {ok, SessionDuration} = db_if_settings:personnel_session_duration(),
                Req2 = cowboy_req:set_resp_cookie(?x_cookie_sid, EncodedSession, Req1, #{path => <<"/">>, max_age => SessionDuration}),
                {#personnel_login_response{
                    result = true,
                    session_id = EncodedSession,
                    user_id = UserId,
                    username = ReqUsername,
                    email = web_util:maybe_null(Email)
                }, Req2};
            {error, Reason} ->
                {#personnel_login_response{result = false, error = Reason}, Req}
        end
    catch
        _C:_R:_S -> {#personnel_login_response{result = false, error = failure}, Req}
    end.

-spec logout_personnel(Request, Req) -> Response when
    Request :: protocol_common:empty(),
    Req :: cowboy_req:req(),
    Response :: {protocol_data:generic_response(), cowboy_req:req()}.

logout_personnel(_Request, #{?m_session := #session{key = {?actor_personnel, SessionId}}} = Req) ->
    web_session:delete(?actor_personnel, SessionId),
    Req1 = cowboy_req:set_resp_cookie(?x_cookie_sid, <<>>, Req, #{path => <<"/">>, max_age => 0}),
    {#generic_response{result = true}, Req1};
logout_personnel(_Request, Req) ->
    {#generic_response{result = false}, Req}.

-spec get_my_personnel_profile(Req) -> Response when
    Req :: cowboy_req:req(),
    Response :: {protocol_personnel:personnel_account_profile(), cowboy_req:req()}.

get_my_personnel_profile(#{?m_session := #session{key = {?actor_personnel, _}, user_id = UserId}} = Req) ->
    {ok, PersonnelAccountProfile} = db_if_personnel:profile(UserId),
    Profile = protocol_db:personnel_account_profile_from_json(PersonnelAccountProfile),
    IsSuperadmin = Profile#personnel_account_profile.is_superadmin orelse access_config:local_admin(),
    {Profile#personnel_account_profile{is_superadmin = IsSuperadmin}, Req}.

%% Local functions
