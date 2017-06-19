-module(dungeon_query_get_battle_context).

-export([get_battle_context/2, get_battle_context/4]).

get_merged_context_map(OriginalCardMap) ->

    DefaultAttrs = #{

        diff 			=> {single, 0},
        attack_disabled => {single, 0},
        cast_disabled 	=> {single, 0},
        is_frozen 		=> {single, 0},
        is_stunned 		=> {single, 0},
        is_disarmed 	=> {single, 0},
        damage_mult 	=> {single, 1},
        critical_mult 	=> {single, 2},
        damage_addon 	=> {single, 0},
        damage_taken 	=> {single, 0},

        outcome 		=> {single, null}
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


get_player_related(Conn, PlayerUUID) ->
    QueryProfile = list_to_binary(["select player_id, player_name, preset_card_id, selected_skills from players where player_id='", PlayerUUID,"';"]),
    {ok, PlayerColumnSpec, Player} = epgsql:squery(Conn,binary_to_list(QueryProfile)),
    hd(util:get_mapped_records(PlayerColumnSpec, Player)).

get_card_related(Conn, CardUUID) ->
    QueryCard = list_to_binary(["select * from cards where card_id='",CardUUID , "';"]),
    {ok, CardColumnSpec, Card} = epgsql:squery(Conn, binary_to_list(QueryCard)),
    CardMapped = hd(util:get_mapped_records_context(CardColumnSpec, Card)),
    get_merged_context_map(CardMapped).


get_battle_context(Conn, PlayerUUID) ->

    #{preset_card_id:=CardID} = PlayerRelated = get_player_related(Conn, PlayerUUID),
    Context = get_card_related(Conn, CardID),
    {ok, maps:merge(Context, PlayerRelated)}.

get_battle_context(Conn, PlayerUUID, CardID, SelectedSkills) ->

    PlayerRelated = get_player_related(Conn, PlayerUUID),
    PlayerModified = PlayerRelated#{preset_card_id:=CardID, selected_skills:=util:array_to_list(SelectedSkills)},
    Context = get_card_related(Conn, CardID),
    {ok, maps:merge(Context, PlayerModified)}.
