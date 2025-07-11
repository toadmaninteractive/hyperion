-module(web_config).

%% Include files

-include("settings.hrl").

%% Exported functions

-export([
    bind_ip/0,
    bind_port/0,
    acceptors/0,
    proxy_enabled/0,
    secure/0,
    cacertfile/0,
    certfile/0,
    keyfile/0,
    url/0,
    upload_dir/0
]).

%% API

-spec bind_ip() ->
    {'ok', inet:ip4_address()} | 'undefined'.

bind_ip() ->
    case application:get_env(?web_app, bind_ip, ?web_ip) of
        {_A, _B, _C, _D} = IpAddressV4 -> {ok, IpAddressV4};
        _ -> undefined
    end.

-spec bind_port() ->
    {'ok', inet:port_number()} | 'undefined'.

bind_port() ->
    case application:get_env(?web_app, bind_port, ?web_port) of
        BindPort when is_integer(BindPort) -> {ok, BindPort};
        _ -> undefined
    end.

-spec acceptors() ->
    {'ok', non_neg_integer()} | 'undefined'.

acceptors() ->
    case application:get_env(?web_app, acceptors, ?web_acceptors) of
        Acceptors when is_integer(Acceptors) -> {ok, Acceptors};
        _ -> undefined
    end.

-spec proxy_enabled() ->
    {'ok', boolean()} | 'undefined'.

proxy_enabled() ->
    case application:get_env(?web_app, proxy_enabled, false) of
        ProxyEnabled when is_boolean(ProxyEnabled) -> {ok, ProxyEnabled};
        _ -> undefined
    end.

-spec secure() ->
    {'ok', boolean()} | 'undefined'.

secure() ->
    case application:get_env(?web_app, secure, false) of
        Secure when is_boolean(Secure) -> {ok, Secure};
        _ -> undefined
    end.

-spec cacertfile() ->
    {'ok', binary()} | 'undefined'.

cacertfile() ->
    case application:get_env(?web_app, cacertfile, undefined) of
        Binary when is_binary(Binary) -> {ok, Binary};
        List when is_list(List) -> {ok, util_binary:to_binary(List)};
        _ -> undefined
    end.

-spec certfile() ->
    {'ok', binary()} | 'undefined'.

certfile() ->
    case application:get_env(?web_app, certfile, undefined) of
        Binary when is_binary(Binary) -> {ok, Binary};
        List when is_list(List) -> {ok, util_binary:to_binary(List)};
        _ -> undefined
    end.

-spec keyfile() ->
    {'ok', binary()} | 'undefined'.

keyfile() ->
    case application:get_env(?web_app, keyfile, undefined) of
        Binary when is_binary(Binary) -> {ok, Binary};
        List when is_list(List) -> {ok, util_binary:to_binary(List)};
        _ -> undefined
    end.

-spec url() ->
    {'ok', binary()} | 'undefined'.

url() ->
    case application:get_env(?web_app, url, undefined) of
        Binary when is_binary(Binary) -> {ok, Binary};
        List when is_list(List) -> {ok, util_binary:to_binary(List)};
        _ -> undefined
    end.

-spec upload_dir() ->
    {'ok', binary()} | 'undefined'.

upload_dir() ->
    case application:get_env(?web_app, upload_dir, undefined) of
        Binary when is_binary(Binary) -> {ok, Binary};
        List when is_list(List) -> {ok, util_binary:to_binary(List)};
        _ -> undefined
    end.

%% Local functions
