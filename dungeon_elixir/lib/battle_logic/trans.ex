defmodule Trans do
	
	# 将指令中的数字按照ref的描述設置
	def trans({:set, imm_number, _, _}, ref) do
		Ref.set(ref, imm_number)
	end

	# 当Inc < 0，且作用的属性是HP时，即是造成伤害，进入伤害处理程序
	def trans({:add, damage, {_, _, _, absorbable, _}, outcome}, {:attr, :state, :hp, player}=dest) when damage < 0 do

	    # 处理护甲减免，护甲减免只在物理攻击部分有效
	    absorbed_damage = case absorbable do
	        :absorbable ->
	            armor_ratio = 1 - Ref.val({:attr, :attr, :armor, player}) * 0.0001
	            damage * armor_ratio
	        _ ->
	            damage
        end

        # 处理不同攻击的结果
	    calculated_damage = case outcome do
	        :critical ->
	            multiplied_critical = Ref.val({:attr, :attr, :critical_multiplier, player})
	            absorbed_damage * multiplied_critical
	        :attack ->
	            absorbed_damage
	        :cast ->
	            damage
	        :resist ->
	            absorbed_damage / 10 * :rand.uniform()
	        _ -> 0
	    end

	    final_damage = calculated_damage * Ref.val({:attr, :attr, :damage_multiplier, player})
	    trans({:set, Ref.val(dest) + final_damage, :none, :none}, dest)
	end

	def trans({:add, increment, _, _outcome}, dest) do
    	trans({:set, Ref.val(dest) + increment, :none, :none}, dest)
    end

	def trans({:add_mul, mul, attack_spec, outcome}, dest) do
    	trans({:add, Ref.val(dest) * mul, attack_spec, outcome}, dest)
    end

	def trans({:add_inc_mul, {inc, mul}, attack_spec, outcome}, dest) do
	    trans({:add, inc * mul, attack_spec, outcome}, dest)
	end

end