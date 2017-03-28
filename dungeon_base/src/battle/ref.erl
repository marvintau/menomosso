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
	Whose#{AttrType:=AttrSet#{ Attr:=round(Val), diff:=round(Val-get(Ref)) }};

set({attr, AttrType, Attr, Whose}, Val) ->
	#{AttrType:=AttrSet} = Whose,
	Whose#{AttrType:=AttrSet#{ Attr:=round(Val) }}.


val({attr, _, _, _}=Ref) ->
	get(Ref);

val({range, Low, High}) ->
    round(Low + rand:uniform() * (High - Low));

val({single, SingleValue}) -> SingleValue.




get({attr, AttrType, Attr, Whose}, O, D) ->
	get({attr, AttrType, Attr, who(Whose, O, D)}).


val({attr, AttrType, Attr, Whose}, O, D) ->
	val({attr, AttrType, Attr, who(Whose, O, D)});
val(Other, _O, _D) ->
	val(Other).

set({attr, AttrType, Attr, Whose}, O, D, Val) ->
	set({attr, AttrType, Attr, who(Whose, O, D)}, Val).
