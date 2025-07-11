%% @author Igor compiler
%% @doc Compiler version: igorc 2.1.1
%% DO NOT EDIT THIS FILE - it is machine generated

-module(web_rest_admin_personnel_group_roles).

-include_lib("igor/include/igor_http.hrl").

-export([
    init/2,
    get_personnel_group_roles_403/1,
    get_personnel_group_roles_404/1,
    get_personnel_group_roles_500/1
]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    Req = handle_method(Method, Req0),
    {ok, Req, Opts}.

handle_method(<<"GET">>, Req) ->
    handle_get(Req);
handle_method(_, Req) ->
    ResponseHeaders = #{<<"Allow">> => <<"GET">>},
    cowboy_req:reply(405, ResponseHeaders, Req).

get_personnel_group_roles_403(ResponseContent403) ->
    throw(#{status_code => 403, response => ResponseContent403}).

get_personnel_group_roles_404(ResponseContent404) ->
    throw(#{status_code => 404, response => ResponseContent404}).

get_personnel_group_roles_500(ResponseContent500) ->
    throw(#{status_code => 500, response => ResponseContent500}).

handle_get(Req) ->
    try
        Id = igor_http:parse_value(cowboy_req:binding(id, Req), long),
        Qs = cowboy_req:parse_qs(Req),
        Needle = igor_http:parse_query(<<"needle">>, Qs, {option, string}),
        OrderBy = igor_http:parse_query(<<"order_by">>, Qs, {custom, fun protocol_personnel:personnel_group_role_order_by_from_string/1}),
        OrderDir = igor_http:parse_query(<<"order_dir">>, Qs, {custom, fun protocol_data:order_direction_from_string/1}),
        Offset = igor_http:parse_query(<<"offset">>, Qs, int),
        Limit = igor_http:parse_query(<<"limit">>, Qs, int),
        Response = web_rest_callback_admin_personnel_roles:get_personnel_group_roles(Id, Needle, OrderBy, OrderDir, Offset, Limit),
        Body = jsx:encode(protocol_data:collection_slice_to_json(Response, {custom, fun protocol_db:personnel_group_role_to_json/1})),
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

