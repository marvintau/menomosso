defmodule CastGenerate do

	# 此module当中的函数主要用于从初始的技能列表中生成可以直接应用于玩家
	# 属性的数据结构上的每一个操作。技能释放概率，以及依据技能本身属性如
	# 物理或魔法为条件来释放的技能，会在这里进行判断。依据玩家状态，比如
	# 被暴击，或闪避，或眩晕中毒等状态不在此处处理。

	# 将技能列表翻译为对应的效果列表，并且在初次读取时就确定技能是否成功释放
	def parse(:single_trans, {index, name, {condition, effect_list} = effect_spec, is_successful}) do
	    for effect <- effect_list, do: {index, name, condition, effect, is_successful}
	end

	def parse(:trans_list, {index, name, {prob, effect_specs}}) do
	    is_successful = :rand.uniform() > prob
	    (for effect_spec <- effect_specs, do: parse(:single_trans, {index, name, effect_spec, is_successful})) |> List.flatten
	end

	def parse(:cast, {index, skill_name}) do
	    {name, groups} = hd(:ets.lookup(:skills, skill_name))
	    (for group <- groups, do: parse(:trans_list, {index, name, group})) |> List.flatten
	end

	def parse(:list, {skill_list}) do
		(for {skill, index} <- :lists.zip(skill_list, :lists.seq(1, length(skill_list))), skill != :none, do: parse(:cast, {index, Skill})) |> List.flatten
	end


	# 确定技能在第几回合释放
	def seq({{:seq_rand, start, {last_least, last_most}, phase}, remaining}, curr_seq, _effects_off, _effects_def) do
	    {{:lists.seq(curr_seq + start, :rand.uniform() * (last_most - last_least) + last_least), phase}, remaining}
	end

	def seq({{:seq_ever, start, null, phase}, remaining}, curr_seq, _effects_off, _effects_def) do
		{{:lists.seq(curr_seq + start, 20), phase}, remaining}
	end

	def seq({{:seq_norm, start, last, phase}, remaining}, curr_seq, _effects_off, _effects_def) do
		{{:lists.seq(curr_seq + start, curr_seq + start + last), phase}, remaining}
	end

	def seq({{:next_offense_norm, last, {attr, move, absorbable, resistable}, phase}, remaining}, curr_seq, effects_off, _effects_def) do

		attack_pattern_check = fn({{_Op, _Operand, {attr_given, move_given, absorbable_given, resistable_given, _}}, _, _}) ->
			((attr_given == attr) or (attr == :none)) and ((move_given == move) or (move == :none)) and
			((absorbable_given == absorbable) or (absorbable == :none)) and ((resistable_given == resistable) or (resistable == :none)) end

		filtered = for {index, _, _, eff, _} <- effects_off, attack_pattern_check.(eff), curr_seq<index, curr_seq + last + 1 >= index, do: index
		{{filtered, phase}, remaining}

	end

	def seq({{:next_cast_norm, last, {attr, move, absorbable, resistable}, phase}, remaining}, curr_seq, _effects_off, effects_def) do

		attack_pattern_check = fn({{_Op, _Operand, {attr_given, move_given, absorbable_given, resistable_given, _}}, _, _}) ->
			((attr_given == attr) or (attr == :none)) and ((move_given == move) or (move == :none)) and
			((absorbable_given == absorbable) or (absorbable == :none)) and ((resistable_given == resistable) or (resistable == :none)) end

		filtered = for {index, _, _, eff, _} <- effects_def, attack_pattern_check.(eff), curr_seq<index, curr_seq + last + 1 >= index, do: index
		{{filtered, phase}, remaining}

	end

	def compress([]), do: []
	def compress(l), do: compress(l,[])
	def compress([head|[]],[head1|tail1]) when head == head1, do: :lists.reverse([head1|tail1])
	def compress([head|[]], acc), do: :lists.reverse([head|acc])

	def compress([head|tail], [head1|tail1]) when head == head1, do: compress(tail, [head1|tail1])
	def compress([head|tail], acc), do: compress(tail,[head|acc])

	def effects(skills_a, skills_b) do
	    effects_a = parse(:list, {skills_a})
	    effects_b = parse(:list, {skills_b})

	    condition_checked_effects_a = for {index, name, condition, effects, is_successful} <- effects_a do
		    {index, name, seq(condition, index, effects_a, effects_b), effects, is_successful}
		end
	    condition_checked_effects_b = for {index, name, condition, effects, is_successful} <- effects_b do
	    	{index, name, seq(condition, index, effects_b, effects_a), effects, is_successful}
	    end

	    {condition_checked_effects_a, condition_checked_effects_b}
	end

	def casts(effects_a, effects_b) do
	    cast_list_a = for {index, skill_name, _, _, is_successful} <- effects_a, do: {index, skill_name, is_successful}
	    cast_list_b = for {index, skill_name, _, _, is_successful} <- effects_b, do: {index, skill_name, is_successful}
	    {compress(cast_list_a), compress(cast_list_b)}
	end

end