-module(player_obtained_card_skill).

-export([add/2, add/5, get/3, get/4, set/4, set/5]).

add(Conn, Map) ->

    Query = list_to_binary([util:add_query(<<"player_obtained_card_skill">>, Map), util:get_query(<<"player_obtained_card_skill">>)]),
    error_logger:info_report(Query),
    [{ok, _}, {ok, ColumnSpec, Result}] = epgsql:squery(Conn, Query),
    Res = util:get_mapped_records(ColumnSpec, Result),

    {ok, Res}.

add(Conn, PlayerCardUUID, PlayerUUID, CardUUID, SkillName) ->

    Query = util:add_query(<<"player_obtained_card_skill">>, #{
        skill_name => SkillName,
        player_card_id=>PlayerCardUUID,
        player_id=>PlayerUUID,
        card_id=>CardUUID
    }),

    {ok, ColumnSpec, Result} = epgsql:squery(Conn, Query),
    [Res] = util:get_mapped_records(ColumnSpec, Result),

    {ok, Res}.

get(Conn, PlayerID, CardID, SkillName) ->

    Query = util:get_query(<<"player_obtained_card_skill">>, #{
        player_id => PlayerID,
        card_id   => CardID,
        skill_name=> SkillName
    }),
    
    {ok, ColumnSpec, Result} = epgsql:squery(Conn, Query),
    [Res] = util:get_mapped_records(ColumnSpec, Result),

    {ok, Res}.

get(Conn, PlayerID, CardID) ->

    Query = util:get_query(<<"player_obtained_card_skill">>, #{
        player_id => PlayerID,
        card_id   => CardID
    }),
    
    {ok, ColumnSpec, Result} = epgsql:squery(Conn, Query),
    Res = util:get_mapped_records(ColumnSpec, Result),

    {ok, Res}.

set(Conn, PlayerMap, PlayerCardUUID, SkillName) ->

    Query = util:set_query(<<"player_obtained_card_skill">>, PlayerMap, #{

        skill_name     => SkillName,
        player_card_id => PlayerCardUUID
    }),

    {ok, _} = epgsql:squery(Conn, Query).

set(Conn, PlayerMap, PlayerUUID, CardUUID, SkillName) ->

    Query = util:set_query(<<"player_obtained_card_skill">>, PlayerMap, #{

        skill_name => SkillName,
        player_id  => PlayerUUID,
        card_id    => CardUUID
    }),

    {ok, _} = epgsql:squery(Conn, Query).
