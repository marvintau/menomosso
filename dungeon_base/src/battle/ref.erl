-module(ref).

-compile({no_auto_import,[get/1]}).
-export([get/1, get/3, set/2, set/4, val/1, val/3, who/3]).

who(off, O, _D) -> O;
who(def, _O, D) -> D.

get({attr, AttrType, Attr, Whose}) ->
	#{AttrType:=#{Attr:=Val}} = Whose,
	Val.


set({attr, AttrType, Attr, Whose}=Ref, Val) when is_number(Val) ->
	#{AttrType:=AttrSet} = Whose,
	Res = Whose#{AttrType:=AttrSet#{ Attr:={single, round(Val)}, diff:={single, round(Val-val(Ref))} }},
	Res;

set({attr, AttrType, Attr, Whose}, Val) ->
	#{AttrType:=AttrSet} = Whose,
	Whose#{AttrType:=AttrSet#{ Attr:=round(Val) }}.


val({range, [Low, High]}) ->
    round(Low + rand:uniform() * (High - Low));

val({single, SingleValue}) -> SingleValue;

val({attr, AttrType, Attr, Whose} = _Ref) ->
	#{AttrType:=#{Attr:=Val}} = Whose,
	val(Val).




get({attr, AttrType, Attr, Whose}, O, D) ->
	get({attr, AttrType, Attr, who(Whose, O, D)}).


val({attr, AttrType, Attr, Whose}, O, D) ->
	val({attr, AttrType, Attr, who(Whose, O, D)});
val(Other, _O, _D) ->
	val(Other).

set({attr, AttrType, Attr, Whose}, O, D, Val) ->
	set({attr, AttrType, Attr, who(Whose, O, D)}, Val).
