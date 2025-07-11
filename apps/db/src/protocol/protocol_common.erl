%% @author Igor compiler
%% @doc Compiler version: igorc 2.1.1
%% DO NOT EDIT THIS FILE - it is machine generated

-module(protocol_common).

-include_lib("stdlib/include/assert.hrl").
-include("protocol_common.hrl").

-export([
    empty_to_json/1,
    empty_from_json/1
]).

-export_type([
    empty/0
]).

-type empty() :: #empty{}.

-spec empty_to_json(empty()) -> igor_json:json_object().

empty_to_json(#empty{}) -> #{}.

-spec empty_from_json(igor_json:json_object()) -> empty().

empty_from_json(_Json) -> #empty{}.

