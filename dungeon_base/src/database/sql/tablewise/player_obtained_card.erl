-module(player_obtained_card).

-export([add/3, get/2, get/3, get_context/3, set/4]).

-export([set_frag/2, set_skill_point/2]).

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
    [Res] = util:get_mapped_records(Columns, Result),
    Res.

get(Conn, PlayerID) ->

    Query = util:get_query(<<"player_obtained_card">>, #{
        player_id=>PlayerID
    }, <<"card_id">>),

    {ok, ColumnSpec, Result} = epgsql:squery(Conn, Query),
    Res = util:get_mapped_records(ColumnSpec, Result),

    Res.

get(Conn, PlayerID, CardID) ->

    Query = util:get_query(<<"player_obtained_card">>, #{
        player_id => PlayerID,
        card_id   => CardID
    }),
   

    {ok, ColumnSpec, Result} = epgsql:squery(Conn, Query),
    [Res] = util:get_mapped_records(ColumnSpec, Result),

    Res.

get_context(Conn, PlayerID, CardID) ->

    Query = util:get_query(<<"player_obtained_card">>, #{
        player_id => PlayerID,
        card_id   => CardID
    }),
    
    {ok, ColumnSpec, Result} = epgsql:squery(Conn, Query),
    [Res] = util:get_mapped_records_context(ColumnSpec, Result),

    Res.

set(Conn, PlayerMap, PlayerUUID, CardUUID) ->

    Query = util:set_query(<<"player_obtained_card">>, PlayerMap, #{
        player_id =>PlayerUUID,
        card_id   =>CardUUID
    }),

    {ok, _} = epgsql:squery(Conn, Query).

set_frag(Conn, {FragIncre, CardID, PlayerUUID}) ->

    Cards = get(Conn, PlayerUUID),
    CardList    = [ID || #{card_id:=ID} <-Cards],

    case lists:member(CardID, CardList) of
        false -> add(Conn, CardID, PlayerUUID);
        _ ->
            SetExp = #{frags=> {e, list_to_binary(["frags+", integer_to_binary(FragIncre)])}},
            Cond   = #{player_id=>PlayerUUID, card_id=>CardID},
            Query  = util:set_query(<<"player">>, SetExp, Cond),
            epgsql:squery(Conn, Query)
    end.

set_skill_point(Conn, {PlayerUUID, CardUUID, SkillPoints}) ->
    SetExp = #{skill_points=>{e, list_to_binary(["skill_points+", integer_to_binary(SkillPoints)])}},
    Cond   = #{player_id=>PlayerUUID, card_id=>CardUUID},
    Query  = util:set_query(<<"player_obtained_card">>, SetExp, Cond),
    epgsql:squery(Conn, Query).


