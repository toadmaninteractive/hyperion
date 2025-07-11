% Roles
-define(role_consumer, consumer).
-define(role_maintainer, maintainer).
-define(role_admin, admin).
-define(role_superadmin, superadmin).

-record(acl, {
    % HTTP verbs
    get :: protocol_db:access_role() | 'undefined',
    post :: protocol_db:access_role() | 'undefined',
    put :: protocol_db:access_role() | 'undefined',
    patch :: protocol_db:access_role() | 'undefined',
    delete :: protocol_db:access_role() | 'undefined',

    % Parameter bindings
    project_id :: atom() | 'undefined',
    parameter_id :: atom() | 'undefined',
    setup_id :: atom() | 'undefined',
    test_case_id :: atom() | 'undefined',
    test_run_id :: atom() | 'undefined',
    test_run_item_id :: atom() | 'undefined',
    owner :: atom() | 'undefined',
    linked_id :: atom() | 'undefined'
}).

-type acl() :: #acl{}.
