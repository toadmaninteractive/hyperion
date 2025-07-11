-module(web_rest_callback_admin_personnel_group).

%% Include files

-include_lib("aplib/include/apmacros.hrl").
-include_lib("db/include/protocol.hrl").
-include("protocol_data.hrl").
-include("protocol_personnel.hrl").
-include("session.hrl").
-include("limits.hrl").

%% Exported functions

-export([
    get_personnel_group/1,
    get_personnel_group_by_name/1,
    get_personnel_groups/5
]).

%% API

-spec get_personnel_group(Id) -> Response when
    Id :: integer(),
    Response :: protocol_db:personnel_group().

get_personnel_group(Id) ->
    {ok, PersonnelGroup} = db_if_personnel_groups:get_one(Id),
    protocol_db:personnel_group_from_json(PersonnelGroup).

-spec get_personnel_group_by_name(Name) -> Response when
    Name :: binary(),
    Response :: protocol_db:personnel_group().

get_personnel_group_by_name(Name) ->
    {ok, PersonnelGroup} = db_if_personnel_groups:get_one_by_name(Name),
    protocol_db:personnel_group_from_json(PersonnelGroup).

-spec get_personnel_groups(Needle, OrderBy, OrderDir, Offset, Limit) -> Response when
    Needle :: binary() | 'undefined',
    OrderBy :: protocol_personnel:personnel_group_order_by(),
    OrderDir :: protocol_data:order_direction(),
    Offset :: non_neg_integer(),
    Limit :: non_neg_integer(),
    Response :: protocol_data:collection_slice(protocol_db:personnel_group()).

get_personnel_groups(Needle, OrderBy, OrderDir, Offset, Limit) ->
    {ok, PersonnelGroups} = db_if_personnel_groups:get(Needle, util_binary:to_binary(OrderBy), util_binary:to_binary(OrderDir), Offset, ?slice_limit(Limit)),
    {ok, Total} = db_if_personnel_groups:get_count(Needle),
    PersonnelGroups1 = lists:map(fun protocol_db:personnel_group_from_json/1, PersonnelGroups),
    #collection_slice{items = PersonnelGroups1, total = Total}.

%% Local functions
