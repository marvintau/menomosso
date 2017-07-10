-module(dungeon_query_battle_record).

-export([store/2, fetch_list/2, fetch/2]).

reform_skill(BinarySkills) ->
	% BinarySkills.
	list_to_binary(["{", string:join(["\""++binary_to_list(Skill)++"\"" || Skill <- BinarySkills], ","), "}"]).

store(Conn, {SelfID, OppoID, SelfCardID, OppoCardID, SelfSelectedSkills, OppoSelectedSkills, SelfWin, BattleRecord}) ->

	
	Query = list_to_binary(["
		insert into player_battle_record( battle_record_id,    self_id,   oppo_id, self_card_id, oppo_card_id,     self_preset_skill, oppo_preset_skill, result, battle_record, last_created)
								  values(uuid_generate_v4(),'",SelfID,"','",OppoID,"','",SelfCardID,"','",OppoCardID,"','",reform_skill(SelfSelectedSkills),"','",reform_skill(OppoSelectedSkills),"','",atom_to_binary(SelfWin, utf8),"','",BattleRecord,"',now());"
	]),

	case epgsql:squery(Conn, Query) of
		{ok, _} ->
			{ok, battle_record_stored};
		Error ->
			{error, Error}
	end.

fetch_list(Conn, {SelfID, OppoID}) ->
	
	Query = list_to_binary(["
     	select battle_record_id, self_id, oppo_id, self_card_id, oppo_card_id, last_created from player_battle_record where self_id=", SelfID, ", oppo_id=", OppoID,";
	"]),

	case epgsql:squery(Conn, Query) of
		{ok, ResultColumns, Result} ->
			{ok, utils:get_mapped_records(ResultColumns, Result)};
		Error ->
			{error, Error}
	end.

fetch(Conn, {BattleRecordID}) ->

	Query = list_to_binary(["
 		select battle_record_id, battle_record from player_battle_record where battle_record_id=", BattleRecordID, ";
	"]),

	case epgsql:squery(Conn, Query) of
		{ok, ResultColumns, Result} ->
			{ok, utils:get_mapped_records(ResultColumns, Result)};
		Error ->
			{error, Error}
	end.
