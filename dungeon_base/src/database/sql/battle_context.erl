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
        pos 			=> {single, 2},
        pos_move 		=> {single, stand},
        diff			=> {single, 0}
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
    Context = get_merged_context_map(card_detail:get_context(Conn, PlayerUUID, CardID)),
    {ok, maps:merge(Context, Player)}.

get(Conn, PlayerUUID, CardID, SelectedSkills) ->

    PlayerRelated = player:get(Conn, PlayerUUID),
    PlayerModified = PlayerRelated#{preset_card_id:=CardID, selected_skills:=SelectedSkills},
    Context = get_merged_context_map(card_detail:get_context(Conn, PlayerUUID, CardID)),
    {ok, maps:merge(Context, PlayerModified)}.
