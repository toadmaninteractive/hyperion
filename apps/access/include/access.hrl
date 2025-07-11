-record(account, {
    username :: binary(),
    password :: binary(),
    allowed_projects :: gb_sets:set(),
    timestamp :: non_neg_integer()
}).

-type account() :: #account{}.

-record(group, {
    name :: binary(),
    members = [] :: [Username :: binary()]
}).

-type group() :: #group{}.
