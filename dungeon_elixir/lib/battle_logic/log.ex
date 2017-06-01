defmodule Log do

	def cast(state, skill_name, is_successful,
		%{
			id: offender_id,
			profession: class_offender,
			player_name: name_offender,
			state: %{
				hp: {_, hp_offender},
				pos: {_, pos_offender},
				pos_move: {_, pos_move_offender}
			},
			attr: %{outcome: {_, _outcome}}} = offender,
		%{
			id: defender_id,
			profession: class_defender,
			player_name: name_defender,
			state: %{
				hp: {_, hp_defender},
				pos: {_, pos_defender},
				pos_move: {_, pos_move_defender}
			}} = defender
	) do

		cast_outcome = case is_successful do
			true -> :successful
			_ -> 	:failed
		end

		:erlang.display({name_offender, {pos_offender, pos_move_offender}, skill_name, cast_outcome, name_defender, {pos_defender, pos_move_defender}})

		%{
			:state => :maps.remove(:offender, state),
			:effect => %{skill_name: skill_name, outcome: cast_outcome, attr: :none, dest: :none, diff: 0},
			offender_id => %{player_name: name_offender, profession: class_offender, role: offender, order: :init, hp: hp_offender, pos: pos_offender, pos_move: pos_move_offender},
			defender_id => %{player_name: name_defender, profession: class_defender, role: defender, order: :init, hp: hp_defender, pos: pos_defender, pos_move: pos_move_defender}
		}
	end


	def effect(
		%{stage: stage} = state, {skill_name, {_, {_, type, attr, who}, _}},
		%{id: offender_id, profession: class_offender, player_name: name_offender, state: %{hp: {_, hp_offender}, pos: {_, pos_offender}, pos_move: {_, pos_move_offender}}, attr: %{outcome: {_, outcome}}} = offender,
		%{id: defender_id, profession: class_defender, player_name: name_defender, state: %{hp: {_, hp_defender}, pos: {_, pos_defender}, pos_move: {_, pos_move_defender}}} = defender
	) do

		init_or_follow = case stage do
			:casting -> :init
			_ 		 -> :follow
		end

		dest = case who do
			:off -> :offender
			_ -> :defender
		end

		:erlang.display({
			{name_offender, hp_offender}, 
			{pos_offender, pos_move_offender},
			skill_name, outcome, attr, dest, Ref.val({:attr, type, :diff, who}, offender, defender),
			{name_defender, hp_defender},
			{pos_defender, pos_move_defender}
		})

		%{
			:state => :maps.remove(offender, state),
			:effect => %{skill_name: skill_name, outcome: outcome, attr: attr, dest: dest, diff: Ref.val({:attr, type, :diff, who}, offender, defender)},
			offender_id => %{player_name: name_offender, profession: class_offender, role: offender, order: init_or_follow, hp: hp_offender, pos: pos_offender, pos_move: pos_move_offender},
			defender_id => %{player_name: name_defender, profession: class_defender, role: defender, order: init_or_follow, hp: hp_defender, pos: pos_defender, pos_move: pos_move_defender}
		}
	end

end