-module(battle).

-author('Yue Marvin Tao').

-export([start/1]).


% ------------- HELPER FUNCTION FOR CHOOSING NEW OFFENDER --------------

toss(#{selected_skills:=[<<"rune_of_the_voplayer_id">>|_], player_id:=A}, _) -> A;
toss(_, #{selected_skills:=[<<"rune_of_the_voplayer_id">>|_], player_id:=B}) -> B;

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
    erlang:display({"=============", begin_settling, Seq+1, Offender, first}),
    S#{stage:=settling, seq:=Seq+1, offender=>Offender};

next_state(#{stage:=settling, seq:=Seq}=S, _, _) ->
    erlang:display({"=============", begin_casting, Seq}),
    S#{stage:=casting}.


apply_move_both(#{stage:=settling}=S, A, B, L) ->
    {OpA, OpB, OpLog}    = trans:effect(S, A, B, L),                             % A上回合遗留下来的出招前的效果
    {Op2B, Op2A, Op2Log} = trans:effect(S, OpB, OpA, OpLog),                     % B上回合遗留下来的出招前效果
    {Op2A, Op2B, Op2Log};

apply_move_both(#{stage:=casting}=S, A, B, L) ->
    {OpA, OpB, OpLog} = trans:cast(S, A, B, L),                                  % A出招
    {OpEffA, OpEffB, OpEffLog} = trans:effect(S, OpA, OpB, OpLog),               % A技能效果
    {OpEff2B, OpEff2A, OpEff2Log} = trans:effect(S#{stage:=counter}, OpEffB, OpEffA, OpEffLog),         % B的反应技能效果
    {OpEff3A, OpEff3B, OpEff3Log} = trans:effect(S#{stage:=append}, OpEff2A, OpEff2B, OpEff2Log),       % A的追加技能效果

    {Op2B, Op2A, Op2Log} = trans:cast(S, OpEff3B, OpEff3A, OpEff3Log),           % B出招
    {Op2EffB, Op2EffA, Op2EffLog} = trans:effect(S, Op2B, Op2A, Op2Log),         % B出招效果
    {Op2Eff2A, Op2Eff2B, Op2Eff2Log} = trans:effect(S#{stage:=counter}, Op2EffA, Op2EffB, Op2EffLog),   % A的反应出招效果
    {Op2Eff3B, Op2Eff3A, Op2Eff3Log} = trans:effect(S#{stage:=append}, Op2Eff2B, Op2Eff2A, Op2Eff2Log), % B的追加技能效果

    {RefreshedA, RefreshedB} = refresh_attributes(Op2Eff3A, Op2Eff3B),
    {RefreshedA, RefreshedB, Op2Eff3Log}.


apply_move_ordered(#{offender:=Off}=S, #{player_id:=Off}=A, B, L) ->
    apply_move_both(S, A, B, L);

apply_move_ordered(#{offender:=Off}=S, A, #{player_id:=Off}=B, L) ->
    {NewB, NewA, NewL} = apply_move_both(S, B, A, L),
    {NewA, NewB, NewL}.


% ------------------------- TERMINATION ------------------------------
% The condition of terminating is one competitor's HP falls below zero.
% When exiting the main loop, the log will be reversed to it's natural
% order.

loop(_, #{selected_skills:=SelectedA, state:=#{hp:={single, HA}}, player_id:=I1}=_A, #{selected_skills:=SelectedB, state:=#{hp:={single, HB}}, player_id:=I2}=_B, Log) when HA < 0 orelse HB < 0 ->

    {Winner, Loser} = if
        HA > HB -> {I1, I2};
        true -> {I2, I1}
    end,

    erlang:display({SelectedA, SelectedB}),
    erlang:display({ended, someone_died}),
    {log, #{records=>lists:reverse(Log), winner=>Winner, loser=>Loser}};

loop(#{seq:=Seq}, #{selected_skills:=SelectedA, state:=#{hp:={single, HA}}=_A, player_id:=I1}, #{selected_skills:=SelectedB, state:=#{hp:={single, HB}}=_B, player_id:=I2}, Log) when Seq > 22->

    {Winner, Loser} = if
        HA > HB -> {I1, I2};
        true -> {I2, I1}
    end,

    erlang:display({SelectedA, SelectedB}),
    erlang:display({ended, no_more_skills}),
    {log, #{records=>lists:reverse(Log), winner=>Winner, loser=>Loser}};

loop(State, A, B, L) ->

    {AppliedA, AppliedB, AppliedLog} = apply_move_ordered(State, A, B, L),

    loop(next_state(State, A, B), AppliedA, AppliedB, AppliedLog).


start({#{selected_skills:=SelectedSkillsA} = A, #{selected_skills:=SelectedSkillsB} = B}) ->

    erlang:display(battle_begins),

    S = next_state(#{seq=>0, stage=>casting}, A, B),

    {EffectsA, EffectsB} = cast:get_effects(SelectedSkillsA, SelectedSkillsB),

    {CastsA, CastsB} = cast:get_casts(EffectsA, EffectsB),

    error_logger:info_report(EffectsA),
    error_logger:info_report(EffectsB),

    loop(S, A#{effects=>EffectsA, casts=>CastsA}, B#{effects=>EffectsB, casts=>CastsB}, []).
