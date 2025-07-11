%% Author: Igor compiler
%% Compiler version: igorc 2.1.1
%% DO NOT EDIT THIS FILE - it is machine generated

-record(create_parameter_request, {
    parent_id :: igor_types:long() | 'undefined',
    dependent_id :: igor_types:long() | 'undefined',
    title :: binary()
}).

-record(rename_parameter_request, {
    new_title :: binary()
}).

-record(add_parameter_value_request, {
    dependent_value :: binary() | 'undefined',
    value :: binary()
}).

-record(rename_parameter_value_request, {
    old_value :: binary(),
    new_value :: binary()
}).

-record(remove_parameter_value_request, {
    dependent_value :: binary() | 'undefined',
    value :: binary()
}).

