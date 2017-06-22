-module(player_obtained_card).

-export([add/3, get/2, get/3, get_context/3, set/3]).

add(Conn, PlayerUUID, CardUUID) ->

    Query = list_to_binary([
        util:add_query(<<"player_obtained_card">>, #{
            player_id=>PlayerUUID,
            card_id=>CardUUID
        }),
        util:get_query(<<"player_obtained_card">>, #{
            player_id=>PlayerUUID,
            card_id=>CardUUID
        })
    ]),

    [{ok, _}, {ok, Columns, Result}] = epgsql:squery(Conn, Query),
    {ok, util:get_mapped_records(Columns, Result)}.

get(Conn, PlayerID) ->

    Query = util:get_query(<<"player_obtained_card">>, #{
        player_id=>PlayerID
    }),

    {ok, ColumnSpec, Result} = epgsql:squery(Conn, Query),
    Res = util:get_mapped_records(ColumnSpec, Result),

    {ok, Res}.

get(Conn, PlayerID, CardID) ->

    Query = util:get_query(<<"player_obtained_card">>, #{
        player_id => PlayerID,
        card_id   => CardID
    }),
    
    {ok, ColumnSpec, Result} = epgsql:squery(Conn, Query),
    [Res] = util:get_mapped_records(ColumnSpec, Result),

    {ok, Res}.

get_context(Conn, PlayerID, CardID) ->

    Query = util:get_query(<<"player_obtained_card">>, #{
        player_id => PlayerID,
        card_id   => CardID
    }),
    
    {ok, ColumnSpec, Result} = epgsql:squery(Conn, Query),
    [Res] = util:get_mapped_records_context(ColumnSpec, Result),

    {ok, Res}.

set(Conn, PlayerMap, {PlayerUUID, CardUUID}) ->

    Query = util:set_query(<<"player_obtained_card">>, PlayerMap, #{
        player_id =>PlayerUUID,
        card_id   =>CardUUID
    }),

    {ok, _} = epgsql:squery(Conn, Query).
