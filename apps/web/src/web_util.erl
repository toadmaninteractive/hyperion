-module(web_util).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("settings.hrl").

%% Exported functions

-export([
    encode_json/1,
    decode_json/1,
    decode_json_safe/2,
    reply_body/5,
    reply_json/4,
    reply_json_error/4,
    reply_json_error/5,
    reply_json_result/4,
    reply_json_explicit/3,
    json_body_partial/2,
    json_body_full/2,
    browser_supports_gzip/1,
    maybe_null/1,
    peer_ipv4/1,
    random_filename/1,
    random_filename/2,
    uuid32/0,
    uuid64/0,
    security_code/0,
    interpolate/2,
    match_prefix/2,
    to_patch/1,
    to_patch/2,
    naive_permutations/1,
    naive_permutations/2,
    naive_permutations/3,
    naive_permutations/4,
    naive_permutations/5,
    naive_permutations/6,
    naive_permutations/7,
    naive_permutations/8,
    naive_permutations/9,
    naive_permutations/10
]).

-define(max_body_length, 128 * 1024).   % 128 KB

%% API

-spec encode_json(Json :: jsx:json_term()) -> jsx:json_text().

encode_json(Json) ->
    jsx:encode(Json).

-spec decode_json(Binary :: jsx:json_text()) -> jsx:json_term().

decode_json(Binary) ->
    jsx:decode(Binary, [return_maps]).

-spec decode_json_safe(Binary :: jsx:json_text(), Default :: jsx:json_term()) -> jsx:json_term().

decode_json_safe(Binary, Default) ->
    try jsx:decode(Binary, [return_maps])
    catch _:_ -> Default
    end.

-spec reply_body(cowboy:http_status(), cowboy:http_headers(), Body :: iodata(), cowboy_req:req(), State :: term()) ->
    {'stop', cowboy_req:req(), term()}.

reply_body(Status, Headers, Body, Req, State) ->
    Req1 = cowboy_req:reply(Status, Headers, Body, Req),
    {stop, Req1, State}.

-spec reply_json(cowboy:http_status(), Json :: jsx:json_term(), cowboy_req:req(), State :: term()) ->
    {'stop', cowboy_req:req(), term()}.

reply_json(Status, Json, Req, State) ->
    Headers = #{<<"content-type">> => <<"application/json">>},
    reply_body(Status, Headers, encode_json(Json), Req, State).

-spec reply_json_error(cowboy:http_status(), Error :: atom() | binary(), cowboy_req:req(), State :: term()) ->
    {'stop', cowboy_req:req(), term()}.

reply_json_error(Status, Error, Req, State) ->
    reply_json_error(Status, Error, undefined, Req, State).

-spec reply_json_error(cowboy:http_status(), Error :: atom() | binary(), Fields :: maps:map() | 'undefined', cowboy_req:req(), State :: term()) ->
    {'stop', cowboy_req:req(), term()}.

reply_json_error(Status, Error, Fields, Req, State) ->
    Headers = #{<<"content-type">> => <<"application/json">>},
    ReplyJson = #{error => Error},
    ReplyJson1 = ?yesno(is_map(Fields) andalso map_size(Fields) > 0, ReplyJson#{fields => Fields}, ReplyJson),
    reply_body(Status, Headers, encode_json(ReplyJson1), Req, State).

-spec reply_json_result(cowboy:http_status(), Result :: boolean(), cowboy_req:req(), State :: term()) ->
    {'stop', cowboy_req:req(), term()}.

reply_json_result(Status, Result, Req, State) ->
    Headers = #{<<"content-type">> => <<"application/json">>},
    ReplyJson = #{result => Result},
    reply_body(Status, Headers, encode_json(ReplyJson), Req, State).

-spec reply_json_explicit(cowboy:http_status(), Json :: jsx:json_term(), cowboy_req:req()) ->
    cowboy_req:req().

reply_json_explicit(Status, Json, Req) ->
    Headers = #{<<"content-type">> => <<"application/json">>},
    Body = encode_json(Json),
    cowboy_req:reply(Status, Headers, Body, Req).

-spec json_body_partial(cowboy_req:req(), Default :: term()) -> Result when
    Result :: {jsx:json_term() | term(), cowboy_req:req()}.

json_body_partial(Req, Default) ->
    case cowboy_req:has_body(Req) of
        true ->
            case read_body_chunk(Req, ?max_body_length) of
                {ok, Body, Req1} -> {decode_json_safe(Body, Default), Req1};
                {error, Req1} -> {Default, Req1}
            end;
        false ->
            {Default, Req}
    end.

-spec json_body_full(cowboy_req:req(), Default :: term()) -> Result when
    Result :: {jsx:json_term() | term(), cowboy_req:req()}.

json_body_full(Req, Default) ->
    case cowboy_req:has_body(Req) of
        true ->
            case read_body(Req, <<>>) of
                {ok, Body, Req1} -> {decode_json_safe(Body, Default), Req1};
                {error, Req1} -> {Default, Req1}
            end;
        false ->
            {Default, Req}
    end.


-spec browser_supports_gzip(Req :: cowboy_req:req()) -> boolean().

browser_supports_gzip(Req) ->
    case cowboy_req:parse_header(<<"accept-encoding">>, Req) of
        Tokens when is_list(Tokens) -> lists:keyfind(<<"gzip">>, 1, Tokens) =/= false;
        _ -> false
    end.

-spec maybe_null(Value :: 'null' | jsx:json_term()) -> jsx:json_term().

maybe_null(null) -> undefined;
maybe_null(Value) -> Value.

-spec peer_ipv4(Req :: cowboy_req:req()) -> binary().

peer_ipv4(Req) ->
    {ok, ProxyEnabled} = web_config:proxy_enabled(),
    case ProxyEnabled of
        true ->
            cowboy_req:header(<<"x-forwarded-for">>, Req, <<"127.0.0.1">>);
        false ->
            {{A, B, C, D}, _} = cowboy_req:peer(Req),
            iolist_to_binary(io_lib:format("~B.~B.~B.~B", [A, B, C, D]))
    end.

-spec random_filename(Extension :: binary()) -> binary().

random_filename(Extension) ->
    random_filename(<<>>, Extension).

-spec random_filename(Prefix :: binary(), Extension :: binary()) -> binary().

random_filename(Prefix, Extension) ->
    Timestamp = integer_to_list(util_time:utc_milliseconds(), 16),
    Timestamp1 = util_binary:to_binary(string:to_lower(Timestamp)),
    Prefix1 = util_binary:to_binary(Prefix),
    Prefix2 = ?yesno(Prefix1 =:= <<>>, <<>>, <<Prefix1/binary, "_">>),
    RandomPart = util_hex:from_binary(crypto:strong_rand_bytes(16)),
    Extension1 = util_binary:to_binary(Extension),
    Extension2 = ?yesno(Extension1 =:= <<>>, <<>>, <<".", Extension1/binary>>),
    <<Prefix2/binary, Timestamp1/binary, "_", RandomPart/binary, Extension2/binary>>.

-spec uuid32() -> binary().

uuid32() ->
    util_hex:from_binary(uuid:get_v4(strong)).

-spec uuid64() -> binary().

uuid64() ->
    Uuid1 = util_hex:from_binary(uuid:get_v4(strong)),
    Uuid2 = util_hex:from_binary(uuid:get_v4(strong)),
    <<Uuid1/binary, Uuid2/binary>>.

-spec security_code() -> binary().

security_code() ->
    A = util_hex:from_binary(crypto:strong_rand_bytes(3)),
    util_binary:to_upper(A).

-spec interpolate(FormHtml :: binary(), Data :: maps:map()) -> binary().

interpolate(FormHtml, Data) when is_map(Data) ->
    maps:fold(fun(K, V, Acc) ->
        K1 = util_binary:to_binary(K),
        V1 = util_binary:to_binary(V),
        binary:replace(Acc, <<"{", K1/binary, "}">>, V1, [global])
    end, FormHtml, Data);
interpolate(FormHtml, _) -> FormHtml.

-spec match_prefix(Path :: binary(), Prefixes :: [binary()]) ->
    {'ok', Prefix :: binary()} | 'nomatch'.

match_prefix(Path, Prefixes) ->
    lists:foldl(fun
        (Prefix, nomatch) ->
            case binary:match(Path, Prefix) of
                {0, _} -> {ok, Prefix};
                _ -> nomatch
            end;
        (_, Acc) ->
            Acc
    end, nomatch, Prefixes).

-spec to_patch(Json :: jsx:json_term()) ->
    jsx:json_term().

to_patch(Json) ->
    to_patch(Json, []).

-spec to_patch(Json :: jsx:json_term(), NullableFields :: [binary()]) ->
    jsx:json_term().

to_patch(Json, NullableFields) ->
    FilterAbsentFun = fun(_K, V) -> V =/= ?null end,
    EnsureNullsFun = fun(K, V) -> ?yesno(lists:member(K, NullableFields) andalso V =:= <<>>, ?null, V) end,
    Patch = maps:filter(FilterAbsentFun, Json),
    maps:map(EnsureNullsFun, Patch).

naive_permutations(L1) ->
    [#{K1 => V1} || {K1, V1} <- L1].

naive_permutations(L1, L2) ->
    [#{K1 => V1, K2 => V2} || {K1, V1} <- L1,
                              {K2, V2} <- L2].

naive_permutations(L1, L2, L3) ->
    [#{K1 => V1, K2 => V2, K3 => V3} || {K1, V1} <- L1,
                                        {K2, V2} <- L2,
                                        {K3, V3} <- L3].

naive_permutations(L1, L2, L3, L4) ->
    [#{K1 => V1, K2 => V2, K3 => V3, K4 => V4} || {K1, V1} <- L1,
                                                  {K2, V2} <- L2,
                                                  {K3, V3} <- L3,
                                                  {K4, V4} <- L4].

naive_permutations(L1, L2, L3, L4, L5) ->
    [#{K1 => V1, K2 => V2, K3 => V3, K4 => V4, K5 => V5} || {K1, V1} <- L1,
                                                            {K2, V2} <- L2,
                                                            {K3, V3} <- L3,
                                                            {K4, V4} <- L4,
                                                            {K5, V5} <- L5].

naive_permutations(L1, L2, L3, L4, L5, L6) ->
    [#{K1 => V1, K2 => V2, K3 => V3, K4 => V4, K5 => V5, K6 => V6} || {K1, V1} <- L1,
                                                                      {K2, V2} <- L2,
                                                                      {K3, V3} <- L3,
                                                                      {K4, V4} <- L4,
                                                                      {K5, V5} <- L5,
                                                                      {K6, V6} <- L6].

naive_permutations(L1, L2, L3, L4, L5, L6, L7) ->
    [#{K1 => V1, K2 => V2, K3 => V3, K4 => V4, K5 => V5, K6 => V6, K7 => V7} || {K1, V1} <- L1,
                                                                                {K2, V2} <- L2,
                                                                                {K3, V3} <- L3,
                                                                                {K4, V4} <- L4,
                                                                                {K5, V5} <- L5,
                                                                                {K6, V6} <- L6,
                                                                                {K7, V7} <- L7].

naive_permutations(L1, L2, L3, L4, L5, L6, L7, L8) ->
    [#{K1 => V1, K2 => V2, K3 => V3, K4 => V4, K5 => V5, K6 => V6, K7 => V7, K8 => V8} || {K1, V1} <- L1,
                                                                                          {K2, V2} <- L2,
                                                                                          {K3, V3} <- L3,
                                                                                          {K4, V4} <- L4,
                                                                                          {K5, V5} <- L5,
                                                                                          {K6, V6} <- L6,
                                                                                          {K7, V7} <- L7,
                                                                                          {K8, V8} <- L8].

naive_permutations(L1, L2, L3, L4, L5, L6, L7, L8, L9) ->
    [#{K1 => V1, K2 => V2, K3 => V3, K4 => V4, K5 => V5, K6 => V6, K7 => V7, K8 => V8, K9 => V9} || {K1, V1} <- L1,
                                                                                                    {K2, V2} <- L2,
                                                                                                    {K3, V3} <- L3,
                                                                                                    {K4, V4} <- L4,
                                                                                                    {K5, V5} <- L5,
                                                                                                    {K6, V6} <- L6,
                                                                                                    {K7, V7} <- L7,
                                                                                                    {K8, V8} <- L8,
                                                                                                    {K9, V9} <- L9].

naive_permutations(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10) ->
    [#{K1 => V1, K2 => V2, K3 => V3, K4 => V4, K5 => V5, K6 => V6, K7 => V7, K8 => V8, K9 => V9, K10 => V10} || {K1, V1} <- L1,
                                                                                                                {K2, V2} <- L2,
                                                                                                                {K3, V3} <- L3,
                                                                                                                {K4, V4} <- L4,
                                                                                                                {K5, V5} <- L5,
                                                                                                                {K6, V6} <- L6,
                                                                                                                {K7, V7} <- L7,
                                                                                                                {K8, V8} <- L8,
                                                                                                                {K9, V9} <- L9,
                                                                                                                {K10, V10} <- L10].

%% Local functions

read_body(Req, Acc) ->
    % Read whole body
    case cowboy_req:read_body(Req) of
        {ok, Data, Req1} -> {ok, <<Acc/binary, Data/binary>>, Req1};
        {more, Data, Req1} -> read_body(Req1, <<Acc/binary, Data/binary>>)
    end.

read_body_chunk(Req, Length) ->
    % Read body chunk
    case cowboy_req:read_body(Req, #{length => Length}) of
        {ok, Data, Req1} -> {ok, Data, Req1};
        {more, _Data, Req1} -> {error, too_long, Req1}
    end.
