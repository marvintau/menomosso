-module(dungeon_query_to_map).

-export([get_listed_player_map/1, get_player_map/1, get_card_map/1, get_profile_map/2, get_card_map_battle/1]).

reform_selected_skills(PresetSkillBinary) ->
    Trimmed = list_to_binary(tl(lists:droplast(binary_to_list(PresetSkillBinary)))),
    [binary_to_atom(Skill, utf8) || Skill <- binary:split(Trimmed, <<",">>, [global])].

get_listed_player_map(
    {ID, Name, ImageName, Association, Expi, Level, Coins, Diamonds, PresetCardID, PresetSkills, Rate, Rank, _, _,
     CardID, CardName, CardImageName, CardLevel, _Stars, Profession, RangeType, HP, _Armor, _Agility, _Hit, _Block, _Dodge, _Resist, _Critical, _AtkType, _AtkMax, _AtkMin, _, _}) ->

    #{
        id => ID, 
        player_name => Name,
        image_name =>ImageName,
        association => Association,
        expi => binary_to_integer(Expi),
        level => binary_to_integer(Level),
        coins => binary_to_integer(Coins),
        diamonds => binary_to_integer(Diamonds),
        preset_card_id => PresetCardID,
        selected_skills => reform_selected_skills(PresetSkills),
        rate => binary_to_integer(Rate),
        rank => binary_to_integer(Rank),

        card_id => CardID,
        card_name => CardName,
        card_image_name => CardImageName,
        card_level=> binary_to_integer(CardLevel),
        hp => binary_to_integer(HP),
        range_type => RangeType,
        profession => Profession

    }.


get_player_map({ID, Name, ImageName, Association, Expi, Level, Coins, Diamonds, PresetCardID, PresetSkills, Rate, Rank, _, _}) ->

    #{
        id => ID, 
        player_name => Name,
        image_name =>ImageName,
        association => Association,
        expi => binary_to_integer(Expi),
        level => binary_to_integer(Level),
        coins => binary_to_integer(Coins),
        diamonds => binary_to_integer(Diamonds),
        preset_card_id => PresetCardID,
        selected_skills => reform_selected_skills(PresetSkills),
        rate => binary_to_integer(Rate),
        rank => binary_to_integer(Rank)
    }.

get_card_map({ID, CardName, ImageName, Level, Stars, Profession, RangeType, HP, Armor, Agility, Hit, Block, Dodge, Resist, Critical, AtkType, AtkMax, AtkMin, _, _, Frags}) ->

    #{id => ID, card_name => CardName, image_name => ImageName, level=> binary_to_integer(Level), stars => binary_to_integer(Stars), profession => Profession,
         range_type => RangeType, hp => binary_to_integer(HP), armor => binary_to_integer(Armor), agility => binary_to_integer(Agility),
         atk_type => AtkType, atk_max => binary_to_integer(AtkMax), atk_min => binary_to_integer(AtkMin),
         hit => binary_to_integer(Hit), block => binary_to_integer(Block), dodge => binary_to_integer(Dodge), resist => binary_to_integer(Resist),
         critical => binary_to_integer(Critical), frags=> binary_to_integer(Frags)}.


get_card_map_battle({_ID, CardName, _Level, _Stars, _ImageName, Profession, RangeType, HP, Armor, Agility, Hit, Block, Dodge, Resist, Critical, AtkType, AtkMax, AtkMin, _, _}) ->

    Attr = #{

        diff => {single, 0},
        attack_disabled => {single, 0},
        cast_disabled => {single, 0},
        is_frozen => {single, 0},
        is_stunned => {single, 0},
        is_disarmed => {single, 0},
        damage_multiplier => {single, 1},
        critical_multiplier => {single, 2},
        damage_addon => {single, 0},
        damage_taken => {single, 0},

        atk_type => {single, binary_to_atom(AtkType, utf8)},
        atk_range => {range, -binary_to_integer(AtkMax), -binary_to_integer(AtkMin)},
        armor => {single, binary_to_integer(Armor)},
        agility => {single, binary_to_integer(Agility)},
        hit => {single, binary_to_integer(Hit)},
        block => {single, binary_to_integer(Block)},
        dodge => {single, binary_to_integer(Dodge)},
        resist => {single, binary_to_integer(Resist)},
        critical => {single, binary_to_integer(Critical)},

        outcome => {single, null}

    },

    erlang:display({range_type, binary_to_atom(RangeType, utf8)}),

    #{
        card_name => CardName,
        profession => Profession,
        range_type => binary_to_atom(RangeType, utf8),

        effects => [],

        state => #{
            hp => {single, binary_to_integer(HP)},
            diff => {single, 0},
            pos => {single, 2},
            pos_move => {single, stand}
        },

        orig_attr => Attr,
        attr => Attr
    }.

get_profile_map(PlayerRes, CardRes) ->

    % UpdatedPlayerRes = setelement(5, PlayerRes, reform_selected_skills(element(5, PlayerRes))),

    #{player_profile => get_player_map(PlayerRes), card_profiles => [get_card_map(Card) || Card <- CardRes]}.
