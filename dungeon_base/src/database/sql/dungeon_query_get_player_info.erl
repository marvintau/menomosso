-module(dungeon_query_get_player_info).

-export([get_player_info/2]).

get_card_skills(Conn, CardID) ->
    Query = list_to_binary(["

        select skill_name, skill_multiple_time, skill_cost from card_skills
        where card_id in('", CardID,"', '00000000-0000-0000-0000-000000000000');
    
    "]),

    {ok, SkillSpec, Skills} = epgsql:squery(Conn, binary_to_list(Query)),
    util:get_mapped_records(SkillSpec, Skills).


get_cards_of_player(Conn, PlayerUUID) ->

    QueryCard = list_to_binary(["
    
        select cards.*, frags, tem.card_level, card_stars, frags_required, coins_required from (
            select * from player_card_info, card_level_up
            where player_id = '", PlayerUUID, "' and player_card_info.card_level=card_level_up.card_level
        ) tem
        inner join cards on cards.card_id=tem.card_id;
    
    "]),

    {ok, CardColumnSpec, CardRes} = epgsql:squery(Conn,binary_to_list(QueryCard)),

    [ CardMap#{skills=> get_card_skills(Conn, CardID)} || #{card_id:=CardID} = CardMap <- util:get_mapped_records(CardColumnSpec, CardRes)].


get_player_info(Conn, PlayerUUID) ->
    QueryProfile = list_to_binary(["select * from players where player_id='", PlayerUUID,"';"]),

    Profile = epgsql:squery(Conn,binary_to_list(QueryProfile)),

    case Profile of
        {ok, _, []} ->
        	{error, player_not_found};

        {ok, PlayerColumnSpec, PlayerRes} ->

            CardMappedRes = get_cards_of_player(Conn, PlayerUUID),
            {ok, #{player_profile => hd(util:get_mapped_records(PlayerColumnSpec, PlayerRes)), card_profiles => CardMappedRes}};

        _ -> {error, get_player_failed}
    end.
