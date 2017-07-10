-module(battle).

-author('Yue Marvin Tao').

-export([start/1]).


% ------------- HELPER FUNCTION FOR CHOOSING NEW OFFENDER --------------

toss(#{selected_skills:=[<<"rune_of_the_void">>|_], player_id:=A}, _) -> A;
toss(_, #{selected_skills:=[<<"rune_of_the_void">>|_], player_id:=B}) -> B;

toss(#{player_id:=A, attr:=#{agility:={single, AgiA}}},
     #{player_id:=B, attr:=#{agility:={single, AgiB}}}) ->
    case rand:uniform() * (AgiA + AgiB) > AgiA of
        true -> B;
        _    -> A
    end.


refresh_attributes(#{orig_attr := OrigA} = PlayerA, #{orig_attr := OrigB} = PlayerB) ->
    {PlayerA#{attr:=OrigA}, PlayerB#{attr:=OrigB}}.


next_state(#{stage:=casting, seq:=Seq}=S, A, B) ->
    Offender = toss(A, B),
    S#{stage:=settling, seq:=Seq+1, offender=>Offender};

next_state(#{stage:=settling}=S, _, _) ->
    S#{stage:=casting}.

apply_move_single(S, A, B, L) ->
    {Ac, Bc, Lc} = trans:cast(S, A, B, L),                                  
    {Ae, Be, Le} = trans:effect(S, Ac, Bc, Lc),               
    {Br, Ar, Lr} = trans:effect(S#{stage:=counter}, Be, Ae, Le),        
    {Aa, Ba, La} = trans:effect(S#{stage:=append}, Ar, Br, Lr),
    {Aa, Ba, La}.

apply_move_both(#{stage:=settling}=S, A, B, L) ->
    {OpA, OpB, OpLog}    = trans:effect(S, A, B, L),
    {Op2B, Op2A, Op2Log} = trans:effect(S, OpB, OpA, OpLog),
    {Op2A, Op2B, Op2Log};

apply_move_both(#{stage:=casting}=S, A, B, L) ->

    {A1, B1, L1} = apply_move_single(S, A, B, L),
    {B2, A2, L2} = apply_move_single(S, B1, A1, L1),

    {Ar, Br} = refresh_attributes(A2, B2),
    {Ar, Br, L2}.


apply_move_ordered(#{offender:=Off}=S, #{player_id:=Off}=A, B, L) ->
    apply_move_both(S, A, B, L);

apply_move_ordered(#{offender:=Off}=S, A, #{player_id:=Off}=B, L) ->
    {NewB, NewA, NewL} = apply_move_both(S, B, A, L),
    {NewA, NewB, NewL}.


% ------------------------- TERMINATION ------------------------------
% The condition of terminating is one competitor's HP falls below zero.
% When exiting the main loop, the log will be reversed to it's natural
% order.

loop(_, #{state:=#{hp:={single, HA}}, player_id:=I1}=_A, #{state:=#{hp:={single, HB}}, player_id:=I2}=_B, Log) when HA < 0 orelse HB < 0 ->

    {Winner, Loser} = if
        HA > HB -> {I1, I2};
        true -> {I2, I1}
    end,

    {log, #{records=>lists:reverse(Log), winner=>Winner, loser=>Loser}};

loop(#{seq:=Seq}, #{state:=#{hp:={single, HA}}, player_id:=I1}, #{state:=#{hp:={single, HB}}, player_id:=I2}, Log) when Seq > 10 ->

    {Winner, Loser} = if
        HA > HB -> {I1, I2};
        true -> {I2, I1}
    end,

    {log, #{records=>lists:reverse(Log), winner=>Winner, loser=>Loser}};

loop(State, A, B, L) ->

    {AppliedA, AppliedB, AppliedLog} = apply_move_ordered(State, A, B, L),

    loop(next_state(State, A, B), AppliedA, AppliedB, AppliedLog).


start({#{selected_skills:=SelectedSkillsA} = A, #{selected_skills:=SelectedSkillsB} = B}) ->

    S = next_state(#{seq=>0, stage=>casting}, A, B),

    {EffectsA, EffectsB} = cast:get_effects(SelectedSkillsA, SelectedSkillsB),

    {CastsA, CastsB} = cast:get_casts(EffectsA, EffectsB),

    loop(S, A#{effects=>EffectsA, casts=>CastsA}, B#{effects=>EffectsB, casts=>CastsB}, []).
