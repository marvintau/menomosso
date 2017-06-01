defmodule Ref do
	
	# 返回角色
	def get_role(:off, o, _d), do: o
	def get_role(:def, _o, d), do: d

	def get({:attr, type, attr, player}) do
		%{^type => %{^attr => value}} = player
		val(value)
	end

	def set({:attr, type, attr, player}=ref, value) when is_number(value) do
		%{^type => type_set} = player
		%{player | type => %{ type_set | attr => {:single, round(value)}, :diff => {:single, round(value-val(ref))} }}
	end

	def set({:attr, type, attr, player}, value) do
		%{^type => type_set} = player
		%{player | type=> %{ type_set | attr=> value }}
	end


	def val({:range, low, high}), do: round(low + :rand.uniform() * (high - low))
	def val({:single, single_value}), do: single_value

	def val({:attr, type, attr, player}) do
		%{^type => %{^attr => value}} = player
		val(value)
	end

	def get({:attr, type, attr, role}, o, d) do
		get({:attr, type, attr, get_role(role, o, d)})
	end


	def val({:attr, type, attr, role}, o, d), do: val({:attr, type, attr, get_role(role, o, d)})
	def val(other, _o, _d), do: val(other)

	def set({:attr, type, attr, role}, o, d, value) do
		set({:attr, type, attr, get_role(role, o, d)}, value)
	end

end