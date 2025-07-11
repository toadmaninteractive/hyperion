%% Author: Igor compiler
%% Compiler version: igorc 2.1.1
%% DO NOT EDIT THIS FILE - it is machine generated

-record(collection, {
    items :: [_T]
}).

-record(collection_slice, {
    total :: igor_types:int(),
    items :: [_T]
}).

-record(tree_node, {
    item :: _T | 'undefined',
    children = [] :: [protocol_data:tree_node(_T)]
}).

-record(generic_response, {
    result :: boolean()
}).

-record(bad_request_error, {
    error :: _T
}).

-record(forbidden_error, {
}).

-record(not_found_error, {
}).

-record(internal_server_error, {
    error :: binary()
}).

