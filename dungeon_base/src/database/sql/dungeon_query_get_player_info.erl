-module(dungeon_query_get_player_info).

-export([get_player_info/2]).

get_cards_of_player(Conn, PlayerUUID) ->

    {ok, Res} = player_obtained_cards:get(Conn, PlayerUUID),
    [ card_detail:get(Conn, PlayerUUID, CardID) || #{card_id:=CardID} <- Res].

     

get_player_info(Conn, PlayerUUID) ->

    {ok, [Player]} = player:get(Conn, PlayerUUID),
    error_logger:info_report(Player),

    Cards = get_cards_of_player(Conn, PlayerUUID),
    error_logger:info_report(Cards),

    {ok, #{player_profile=>Player, card_profiles=>Cards}}.

