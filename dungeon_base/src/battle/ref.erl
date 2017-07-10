-module(ref).

-compile({no_auto_import,[get/1]}).
-export([get/1, get/3, set/2, set/4, val/1, val/3, who/3]).

who(off, O, _D) -> O;
who(def, _O, D) -> D.

get({attr, Attr, Whose}) ->

    case Whose of
        #{attr  :=#{Attr:=Val}} -> Val;
        #{state :=#{Attr:=Val}} -> Val
    end.


set({attr, Attr, Whose}=Ref, NewVal) when is_number(NewVal) ->

    case Whose of
        #{state:=#{Attr:=_}=AttrSet, attr:=AttrTypeset} ->
            Whose#{state:=AttrSet#{
                            Attr:={single, round(NewVal)}
                           },
                   attr:=AttrTypeset#{
                           diff:={single, round(NewVal-val(Ref))}
                          }
                  };
        #{attr :=#{Attr:=_}=AttrSet} ->
            Whose#{attr:=AttrSet#{
                           Attr:={single, round(NewVal)},
                           diff:={single, round(NewVal-val(Ref))}
                          }
                  }
    end;

set({attr, Attr, Whose}, Val) ->

    case Whose of
        #{attr :=#{Attr:=_} = AttrSet} ->
            Whose#{attr  :=AttrSet#{Attr:=Val}};
        #{state :=#{Attr:=_} = AttrSet} ->
            Whose#{state :=AttrSet#{Attr:=Val}}
    end.



val({range, [Low, High]}) ->
    round(Low + rand:uniform() * (High - Low));

val({single, SingleValue}) -> SingleValue;

val({attr, Attr, Whose} = _Ref) ->

    case Whose of
        #{attr  :=#{Attr:=Val}} -> val(Val);
        #{state :=#{Attr:=Val}} -> val(Val)
    end.


get({attr, Attr, Whose}, O, D) ->
    get({attr, Attr, who(Whose, O, D)}).


val({attr, Attr, Whose}, O, D) ->
    val({attr, Attr, who(Whose, O, D)});
val(Other, _O, _D) ->
    val(Other).

set({attr, Attr, Whose}, O, D, Val) ->
    set({attr, Attr, who(Whose, O, D)}, Val).
