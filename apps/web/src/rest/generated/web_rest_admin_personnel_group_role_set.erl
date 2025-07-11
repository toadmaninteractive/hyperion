%% @author Igor compiler
%% @doc Compiler version: igorc 2.1.1
%% DO NOT EDIT THIS FILE - it is machine generated

-module(web_rest_admin_personnel_group_role_set).

-include_lib("igor/include/igor_http.hrl").

-export([
    init/2,
    set_personnel_group_role_403/1,
    set_personnel_group_role_404/1,
    set_personnel_group_role_500/1,
    reset_personnel_group_role_403/1,
    reset_personnel_group_role_404/1,
    reset_personnel_group_role_500/1
]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    Req = handle_method(Method, Req0),
    {ok, Req, Opts}.

handle_method(<<"PUT">>, Req) ->
    case cowboy_req:has_body(Req) of
        true -> handle_put(Req);
        false -> cowboy_req:reply(400, Req)
    end;
handle_method(<<"DELETE">>, Req) ->
    handle_delete(Req);
handle_method(_, Req) ->
    ResponseHeaders = #{<<"Allow">> => <<"PUT, DELETE">>},
    cowboy_req:reply(405, ResponseHeaders, Req).

set_personnel_group_role_403(ResponseContent403) ->
    throw(#{status_code => 403, response => ResponseContent403}).

set_personnel_group_role_404(ResponseContent404) ->
    throw(#{status_code => 404, response => ResponseContent404}).

set_personnel_group_role_500(ResponseContent500) ->
    throw(#{status_code => 500, response => ResponseContent500}).

handle_put(Req) ->
    try
        {ok, RequestBody, Req1} = cowboy_req:read_body(Req),
        Request = protocol_personnel:access_role_update_request_from_json(jsx:decode(RequestBody, [return_maps])),
        Id = igor_http:parse_value(cowboy_req:binding(id, Req1), long),
        Project = igor_http:parse_value(cowboy_req:binding(project, Req1), long),
        Response = web_rest_callback_admin_personnel_roles:set_personnel_group_role(Request, Id, Project),
        Body = jsx:encode(protocol_data:generic_response_to_json(Response)),
        ResponseHeaders = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
        cowboy_req:reply(200, ResponseHeaders, Body, Req1)
    catch
        #{status_code := 403, response := ResponseContent403} ->
            ResponseContent403Body = jsx:encode(protocol_data:forbidden_error_to_json(ResponseContent403)),
            ResponseHeaders403 = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
            cowboy_req:reply(403, ResponseHeaders403, ResponseContent403Body, Req);
        #{status_code := 404, response := ResponseContent404} ->
            ResponseContent404Body = jsx:encode(protocol_data:not_found_error_to_json(ResponseContent404)),
            ResponseHeaders404 = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
            cowboy_req:reply(404, ResponseHeaders404, ResponseContent404Body, Req);
        #{status_code := 500, response := ResponseContent500} ->
            ResponseContent500Body = jsx:encode(protocol_data:internal_server_error_to_json(ResponseContent500)),
            ResponseHeaders500 = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
            cowboy_req:reply(500, ResponseHeaders500, ResponseContent500Body, Req);
        #bad_request{} ->
            cowboy_req:reply(400, Req)
    end.

reset_personnel_group_role_403(ResponseContent403) ->
    throw(#{status_code => 403, response => ResponseContent403}).

reset_personnel_group_role_404(ResponseContent404) ->
    throw(#{status_code => 404, response => ResponseContent404}).

reset_personnel_group_role_500(ResponseContent500) ->
    throw(#{status_code => 500, response => ResponseContent500}).

handle_delete(Req) ->
    try
        Id = igor_http:parse_value(cowboy_req:binding(id, Req), long),
        Project = igor_http:parse_value(cowboy_req:binding(project, Req), long),
        Response = web_rest_callback_admin_personnel_roles:reset_personnel_group_role(Id, Project),
        Body = jsx:encode(protocol_data:generic_response_to_json(Response)),
        ResponseHeaders = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
        cowboy_req:reply(200, ResponseHeaders, Body, Req)
    catch
        #{status_code := 403, response := ResponseContent403} ->
            ResponseContent403Body = jsx:encode(protocol_data:forbidden_error_to_json(ResponseContent403)),
            ResponseHeaders403 = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
            cowboy_req:reply(403, ResponseHeaders403, ResponseContent403Body, Req);
        #{status_code := 404, response := ResponseContent404} ->
            ResponseContent404Body = jsx:encode(protocol_data:not_found_error_to_json(ResponseContent404)),
            ResponseHeaders404 = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
            cowboy_req:reply(404, ResponseHeaders404, ResponseContent404Body, Req);
        #{status_code := 500, response := ResponseContent500} ->
            ResponseContent500Body = jsx:encode(protocol_data:internal_server_error_to_json(ResponseContent500)),
            ResponseHeaders500 = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
            cowboy_req:reply(500, ResponseHeaders500, ResponseContent500Body, Req);
        #bad_request{} ->
            cowboy_req:reply(400, Req)
    end.

