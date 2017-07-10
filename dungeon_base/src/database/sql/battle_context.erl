-module(battle_context).

-export([get/2, get/4]).

get_merged_context_map(OriginalCardMap) ->

    DefaultAttrs = #{

        diff 		=> {single, 0},
        attack_disabled => {single, 0},
        cast_disabled 	=> {single, 0},
        is_frozen 	=> {single, 0},
        is_stunned 	=> {single, 0},
        is_disarmed 	=> {single, 0},
        damage_mult 	=> {single, 1},
        critical_mult 	=> {single, 2},
        damage_addon 	=> {single, 0},
        damage_taken 	=> {single, 0},

        outcome 	=> {single, null}
    },

    DefaultStates = #{
        diff		=> {single, 0}
    },

    NewAttrs = maps:merge(DefaultAttrs, maps:remove(hp, OriginalCardMap)),
    NewStates = DefaultStates#{hp => maps:get(hp, OriginalCardMap)},

    #{
    	state => NewStates,
    	attr  => NewAttrs,
    	orig_attr => NewAttrs
    }.


get(Conn, PlayerUUID) ->

    #{preset_card_id:=CardID} =Player = player:get(Conn, PlayerUUID),
    #{selected_skills:=SelectedSkills} = player_obtained_card:get(Conn, PlayerUUID, CardID),
    PlayerWithSkills = Player#{selected_skills=>SelectedSkills},

    Context = get_merged_context_map(card_detail:get_context(Conn, PlayerUUID, CardID)),
    {ok, maps:merge(Context, PlayerWithSkills)}.

get(Conn, PlayerUUID, CardID, SelectedSkills) ->

    PlayerRelated = player:get(Conn, PlayerUUID),
    PlayerModified = PlayerRelated#{preset_card_id:=CardID},
    Context = get_merged_context_map(card_detail:get_context(Conn, PlayerUUID, CardID)),
    MergedContext = maps:merge(Context, PlayerModified),
    MergedWithSkills = MergedContext#{selected_skills=>SelectedSkills},
    {ok, MergedWithSkills}.
