%% @author Igor compiler
%% @doc Compiler version: igorc 2.1.1
%% DO NOT EDIT THIS FILE - it is machine generated

-module(web_rest_test_case_tree).

-include_lib("igor/include/igor_http.hrl").

-export([
    init/2,
    get_test_case_tree_403/1,
    get_test_case_tree_500/1
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

get_test_case_tree_403(ResponseContent403) ->
    throw(#{status_code => 403, response => ResponseContent403}).

get_test_case_tree_500(ResponseContent500) ->
    throw(#{status_code => 500, response => ResponseContent500}).

handle_get(Req) ->
    try
        ProjectId = igor_http:parse_value(cowboy_req:binding(project_id, Req), long),
        ResponseContent = web_rest_callback_test_case:get_test_case_tree(ProjectId),
        Body = jsx:encode(protocol_data:collection_to_json(ResponseContent, {custom, fun(V) -> protocol_data:tree_node_to_json(V, {custom, fun protocol_db:test_case_to_json/1}) end})),
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

