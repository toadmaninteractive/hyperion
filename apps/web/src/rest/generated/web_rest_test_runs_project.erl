%% @author Igor compiler
%% @doc Compiler version: igorc 2.1.1
%% DO NOT EDIT THIS FILE - it is machine generated

-module(web_rest_test_runs_project).

-include_lib("igor/include/igor_http.hrl").

-export([
    init/2,
    get_test_runs_403/1,
    get_test_runs_500/1
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

get_test_runs_403(ResponseContent403) ->
    throw(#{status_code => 403, response => ResponseContent403}).

get_test_runs_500(ResponseContent500) ->
    throw(#{status_code => 500, response => ResponseContent500}).

handle_get(Req) ->
    try
        ProjectId = igor_http:parse_value(cowboy_req:binding(project_id, Req), long),
        Qs = cowboy_req:parse_qs(Req),
        OrderBy = igor_http:parse_query(<<"order_by">>, Qs, {custom, fun protocol_test_run:test_run_order_by_from_string/1}),
        OrderDir = igor_http:parse_query(<<"order_dir">>, Qs, {custom, fun protocol_data:order_direction_from_string/1}),
        Offset = igor_http:parse_query(<<"offset">>, Qs, long),
        Limit = igor_http:parse_query(<<"limit">>, Qs, long),
        Status = igor_http:parse_query(<<"status">>, Qs, {option, {custom, fun protocol_db:test_run_status_from_string/1}}),
        ResponseContent = web_rest_callback_test_run:get_test_runs(ProjectId, OrderBy, OrderDir, Offset, Limit, Status),
        Body = jsx:encode(protocol_data:collection_slice_to_json(ResponseContent, {custom, fun protocol_db:test_run_to_json/1})),
        ResponseHeaders = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
        cowboy_req:reply(200, ResponseHeaders, Body, Req)
    catch
        #{status_code := 403, response := ResponseContent403} ->
            ResponseContent403Body = jsx:encode(protocol_data:forbidden_error_to_json(ResponseContent403)),
            ResponseHeaders403 = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
            cowboy_req:reply(403, ResponseHeaders403, ResponseContent403Body, Req);
        #{status_code := 500, response := ResponseContent500} ->
            ResponseContent500Body = jsx:encode(protocol_data:internal_server_error_to_json(ResponseContent500)),
            ResponseHeaders500 = #{<<"Content-Type">> => <<"application/json; charset=utf-8">>},
            cowboy_req:reply(500, ResponseHeaders500, ResponseContent500Body, Req);
        #bad_request{} ->
            cowboy_req:reply(400, Req)
    end.

