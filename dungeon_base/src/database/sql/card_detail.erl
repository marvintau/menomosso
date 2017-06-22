-module(card_detail).

-export([get/3, get_context/3]).

get_skills(Conn, PlayerUUID, CardID) ->
    {ok, Res} = player_obtained_card_skill:get(Conn, PlayerUUID, CardID),

    [#{skill_name=>SkillName, skill_cost=>SkillCost, skill_multiple_time=>SkillMultipleTime} || #{skill_name:=SkillName, skill_cost:=SkillCost, skill_multiple_time:=SkillMultipleTime} <- Res].


merge_four_maps(Card, Type, Spec, Skills) ->

    Map1 = maps:merge(Card, Type),
    Map2 = maps:merge(Map1, Spec),
    Map3 = maps:merge(Map2, Skills),
    Map3.    

get_card_profile(Conn, Card, PlayerID, CardID, CardLevel) ->

    Type = card_type:get(Conn, CardID),
    Spec = card_level_spec:get(Conn, CardID, CardLevel),
    Skills = #{skills=>get_skills(Conn, PlayerID, CardID)},

    merge_four_maps(Card, Type, Spec, Skills).

get_card_profile_context(Conn, Card, CardID, CardLevel) ->

    Type = card_type:get_context(Conn, CardID),
    Spec = card_level_spec:get_context(Conn, CardID, CardLevel),
    
    merge_four_maps(Card, Type, Spec, #{}).

get(Conn, PlayerUUID, CardID) ->

    {ok, #{card_level:=CardLevel}=Card} = player_obtained_card:get(Conn, PlayerUUID, CardID),

    get_card_profile(Conn, Card, PlayerUUID, CardID, CardLevel).

get_context(Conn, PlayerUUID, CardID) ->

    {ok, #{card_level:={single, CardLevel}}=Card} = player_obtained_card:get_context(Conn, PlayerUUID, CardID),

    get_card_profile_context(Conn, Card, CardID, CardLevel).
