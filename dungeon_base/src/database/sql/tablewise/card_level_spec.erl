-module(card_level_spec).

-export([get/3, get_context/3]).

get(Conn, CardUUID, CardLevel) ->
    Query = list_to_binary(["select * from card_level_spec where card_id='", CardUUID, "' and level=", integer_to_binary(CardLevel), ";"]),
    {ok, ColumnSpec, Result} = epgsql:squery(Conn, Query),
    [CardLevelRes] = util:get_mapped_records(ColumnSpec, Result),

    CardLevelRes.

get_context(Conn, CardUUID, CardLevel) ->
    Query = list_to_binary(["select * from card_level_spec where card_id='", CardUUID, "' and level=", integer_to_binary(CardLevel), ";"]),
    {ok, ColumnSpec, Result} = epgsql:squery(Conn, Query),
    [CardLevelRes] = util:get_mapped_records_context(ColumnSpec, Result),

    CardLevelRes.


