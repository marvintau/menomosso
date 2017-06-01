defmodule Move do

	def repose(%{state: state_offender}=offender, %{state: state_defender}=defender, {:stand, _}) do
		{
			%{offender| state: %{state_offender| pos_move: {:single, :stand}}},
			%{defender| state: %{state_defender| pos_move: {:single, :stand}}}
		}
	end

	def repose(
		%{state: %{pos: {:single, pos_offender}}=state_offender, attr: %{outcome: {:single, outcome}}} = offender,
		%{state: %{pos: {:single, pos_defender}, hp: {:single, hp_defender}}=state_defender, attr: %{is_frozen: {:single, is_frozen}, is_disarmed: {:single, is_disarmed}, is_stunned: {:single, is_stunned}}} = defender,
		{repos_type, is_blowout_enabled}) do

    	# % 根据近战远战类型决定追逃动作
    	{new_pos_offender, new_pos_defender, new_pos_move_offender, new_pos_move_defender} = case repos_type do

	        # % 只有当pos_offender + pos_defender == 5 的时候才是格斗距离，比这个小说明远了
	        :chase when (pos_offender + pos_defender) < 5 ->
	        	{5 - pos_defender, pos_defender, :chase, :not_assigned_yet}

	        # % 如果不是，则说明正是格斗距离，不动
	        :chase ->
	        	{pos_offender, pos_defender, :stand, :not_assigned_yet}

	        # % 只有当 4 >= pos_offender + pos_defender >= 3的时候才是远战格斗距离，比这个再远需要追上
	        :back when (pos_offender + pos_defender) < 3 ->
	        	{pos_offender + 1, pos_defender, :chase, :not_assigned_yet}

	        # % 如果是近战的距离需要跳开
	        :back when pos_offender + pos_defender == 5 ->
	        	{pos_offender - 1, pos_defender, :back_jump, :not_assigned_yet}

	        	:back ->
	        		{pos_offender, pos_defender, :stand, :not_assigned_yet}
	        	end

    	# % 决定击飞动作
    	blown_prob = :rand.uniform()

    # % 如果抽中随机数，且被攻击者不在版边，并且被攻击者不处在冰冻/眩晕/缴械状态，并且被攻击者的反应不是
	    {new_pos_defender2, new_pos_move_defender2} = case {new_pos_defender, new_pos_move_defender} do
	    	{_, :not_assigned_yet} when
	    		is_blowout_enabled and (blown_prob > 0.9) and (is_frozen == 0) and (is_disarmed == 0) and (is_stunned == 0)
	    		and (outcome != :dodge) and (outcome != :block) and (outcome != :resist) or (hp_defender <= 0) ->
	    		{new_pos_defender - 1, :blown_out}

			{_, :not_assigned_yet} ->
				{new_pos_defender, :stand}
			_ ->
				{new_pos_defender, new_pos_move_defender}
		end

		{%{offender| state: %{state_offender| pos: {:single, new_pos_offender}, pos_move: {:single, new_pos_move_offender}}},
	  	 %{defender| state: %{state_defender| pos: {:single, new_pos_defender2}, pos_move: {:single, new_pos_move_defender2}}}}
	end


	def bin(given_val, [bin|bins]), do: bin(given_val, bin, bins, 1)

	def bin(_, _, [], ith), do: ith
	def bin(given_val, accum, _, ith) when given_val < accum, do: ith
	def bin(given_val, accum, [bin|bins], ith), do: bin(given_val, accum+bin, bins, ith+1)

	def roulette(attack_spec,
		%{attr: %{hit: {:single, hit}, critical: {:single, crit}}},
		%{attr: %{resist: {:single, res}, block: {:single, blo}, dodge: {:single, dod}}} ) do

		max_roulette_limit = 120

	    # % 获得攻击属性（魔法／物理），放招类型（普攻／技能），是否可以抵抗，是否护甲减免（不考虑），技能失败概率
	    {attr_type, move_type, resistable, _Absorbable, fail}  = attack_spec

	    # % 实际的抗性：如果技能不可抵抗，那么实际的魔抗值为0
	    actual_res = if resistable == :resistable, do: res, else: 0

	    # % 实际的闪避：原始的闪避减去了命中加成，直到减到0为止
	    actual_dod = if dod - hit > 0, do: dod - hit, else: 0

	    # % 实际的失败概率：最大为0.999，最小为0
	    actual_fail = if fail > 0.999, do: 0.999, else: if fail <= 0, do: 0, else: fail

	    # % 通过攻击属性和放招类型，获得最终的闪避／抗性／格挡／暴击／失败概率
	    {dodge, resist, block, critical, failure_rate} = case {attr_type, move_type} do

	    	{:magic, :attack}    -> {0, 		 actual_res, 0, 		crit, actual_fail}

	    	{:physical, :attack} -> {actual_dod, 0, 		 blo, 		crit, actual_fail}
	    	
	    	{:magic, :cast}      -> {0, 		 actual_res, 0, 		   0, actual_fail}
	    	
	    	{:physical, :cast}   -> {0,					  0, 0, 		crit, actual_fail}
	    	
	    	_ -> {0, 0, 0, 0, 0}

	    end

	    # % 生成轮盘：
	    # % 1) 计算出普通攻击／技能的区间
	    # % 2) 把其它结果加入
	    # % 3) 按照失败概率稀释原有轮盘，加入失败结果的区间

		normal   = max_roulette_limit - dodge - resist - block - critical
		roulette = [normal, dodge, resist, block, critical]
		roulette_with_failure = [failure_rate/(1-failure_rate) * max_roulette_limit | roulette]

		# % 抽随机数
		binned = bin(:rand.uniform() * max_roulette_limit / (1 - failure_rate), roulette_with_failure)

		# % 得到结果，如果是技能就是cast，平砍是attack
		result = elem({:failed, move_type, :dodge, :resist, :block, :critical}, binned)
		result
	end


	def apply({{opcode, oper, attack_spec}, {:attr, type, attr, role}, repose_type}, o, d) do

	    # % 获得双方的操作数，因为操作数只能有一个或者两个，此处需要先从立即数或玩家属性结构中获得数字
	    ref_oprand = case oper do
	    	{ref} -> Ref.val(ref, o, d)
	    	{ref1, ref2} -> {Ref.val(ref1, o, d), Ref.val(ref2, o, d)}
	    end

	    # % 得到转盘结果
	    outcome = roulette(attack_spec, o, d)

	    # % 将转盘结果加入玩家context，并按结果计算伤害／技能效果，把结果保存在Transed里面
	    role_context = Ref.get_role(role, o, d)
	    transed_role_context = Trans.trans({opcode, ref_oprand, attack_spec, outcome}, {:attr, type, attr, role_context})

	    # 将操作应用在role所指定的对象上
	    {%{attr: attr_offender} = transed_offender, transed_defender} = case role do
	    	:off ->  {transed_role_context, d}
	    	:def ->  {o, transed_role_context}
	    end

	    repose(%{transed_offender | attr: %{ attr_offender | outcome: {:single, outcome}}}, transed_defender, repose_type)
	end

end