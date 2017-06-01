defmodule CastApply do

	# 用来比较技能所给定的依赖于玩家状态的生效条件，和实际玩家状态的比较
	defp comp({val, :==, ref}, offender, defender), do: val == Ref.val(ref, offender, defender)
	defp comp({val, :>, ref}, offender, defender), do: val > Ref.val(ref, offender, defender)
	defp comp({val, :<, ref}, offender, defender), do: val < Ref.val(ref, offender, defender)


	defp comps(cons_list, offender, defender), do: comps(cons_list, offender, defender, :true)

	defp comps([condition | rem_conds], offender, defender, true_value) do
		comps(rem_conds, offender, defender, true_value and comp(condition, offender, defender))
	end 

	defp comps([], _, _, true_value), do: true_value

	defp seq_check({seq_list, stage}, %{seq: curr_seq, stage: curr_stage}, %{attr: %{cast_disabled: {:single, cast_disabled}}}) do
	    :lists.any(&(curr_seq == &1), seq_list) and (stage == curr_stage) and not ((stage == :casting) and (cast_disabled != 0))
	end

	def check({seq_cond, cons_list}, state, offender, defender) do
	    seq_check(seq_cond, state, offender) and comps(cons_list, offender, defender)
	end

	
	def cast(state, %{casts: casts}=offender, defender, log), do: cast(state, offender, defender, log, casts)

	def cast(_state, %{state: %{hp: {:single, hp1}}}=offender, %{state: %{hp: {:single, hp2}}}=defender, log, _) when (hp1 <= 0) or (hp2 <= 0), do: {offender, defender, log}

	def cast(_state, offender, defender, log, []), do: {offender, defender, log}

	def cast(%{stage: :casting}, %{attr: %{cast_disabled: {:single, cast_disabled}}}=offender, defender, log, _) when cast_disabled != 0, do: {offender, defender, log}

	def cast(%{seq: seq}=state, %{state: state_offender}=offender, %{state: state_defender}=defender, log, [{seq_index, skill_name, is_successful} | remaining]) do

		stand_offender = %{offender| state: %{state_offender| pos_move: {:single, :stand}}}
		stand_defender = %{defender| state: %{state_defender| pos_move: {:single, :stand}}}

		new_log = case seq == seq_index do
			true ->
				[Log.cast(state, skill_name, is_successful, stand_offender, stand_defender) | log]
			_ ->
				log
		end

		cast(state, stand_offender, stand_defender, new_log, remaining)
	end


	def effect(state, %{effects: effects}=offender, defender, log) do
		effect(state, offender, defender, log, effects)
	end

	def effect(_state, %{state: %{hp: {single, hp1}}}=offender, %{state: %{hp: {single, hp2}}}=defender, log, _) when (hp1 <= 0) or (hp2 <= 0), do: {offender, defender, log}

	def effect(_state, offender, defender, log, []), do: {offender, defender, log}

	def effect(state, offender, defender, log, [ {_Index, name, conds, trans, _Success} | remaining]) do

		{new_offender, new_defender, new_log} = case check(conds, state, offender, defender) do
			true ->
				{ transedO, transedD} = Move.apply(trans, offender, defender)
				translog = Log.effect(state, {name, trans}, transedO, transedD)
				{transedO, transedD, [translog | log]}
			_	->
				{offender, defender, log}
		end

		effect(state, new_offender, new_defender, new_log, remaining)
	end

end