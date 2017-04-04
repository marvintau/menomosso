-module(battle).

-author('Yue Marvin Tao').

-export([start/1]).


% ------------- HELPER FUNCTION FOR CHOOSING NEW OFFENDER --------------

toss(#{selected_skills:=[rune_of_the_void|_], id:=A}, _) -> A;
toss(_, #{selected_skills:=[rune_of_the_void|_], id:=B}) -> B;

toss(#{id:=A, attr:=#{agility:={single, AgiA}}},
     #{id:=B, attr:=#{agility:={single, AgiB}}}) ->
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

next_state(#{stage:=settling}=S, _, _) ->
    erlang:display({"=============", begin_casting}),
    S#{stage:=casting}.


apply_move_both(#{stage:=settling}=S, A, B, L) ->
    {OpA, OpB, OpLog}    = cast:effect(S, A, B, L),                             % A上回合遗留下来的出招前的效果
    {Op2B, Op2A, Op2Log} = cast:effect(S, OpB, OpA, OpLog),                     % B上回合遗留下来的出招前效果
    {Op2A, Op2B, Op2Log};

apply_move_both(#{stage:=casting}=S, A, B, L) ->
    {OpA, OpB, OpLog} = cast:cast(S, A, B, L),                                  % A出招
    {OpEffA, OpEffB, OpEffLog} = cast:effect(S, OpA, OpB, OpLog),               % A技能效果
    {OpEff2B, OpEff2A, OpEff2Log} = cast:effect(S#{stage:=counter}, OpEffB, OpEffA, OpEffLog),   % B的反应技能效果

    {Op2B, Op2A, Op2Log} = cast:cast(S, OpEff2B, OpEff2A, OpEff2Log),           % B出招
    {Op2EffB, Op2EffA, Op2EffLog} = cast:effect(S, Op2B, Op2A, Op2Log),         % B的追加技能效果
    {Op2Eff2A, Op2Eff2B, Op2Eff2Log} = cast:effect(S#{stage:=counter}, Op2EffA, Op2EffB, Op2EffLog), % A的反应出招效果

    {RefreshedA, RefreshedB} = refresh_attributes(Op2Eff2A, Op2Eff2B),
    {RefreshedA, RefreshedB, Op2Eff2Log}.


apply_move_ordered(#{offender:=Off}=S, #{id:=Off}=A, B, L) ->
    apply_move_both(S, A, B, L);

apply_move_ordered(#{offender:=Off}=S, A, #{id:=Off}=B, L) ->
    {NewB, NewA, NewL} = apply_move_both(S, B, A, L),
    {NewA, NewB, NewL}.


% ------------------------- TERMINATION ------------------------------
% The condition of terminating is one competitor's HP falls below zero.
% When exiting the main loop, the log will be reversed to it's natural
% order.

loop(_, #{state:=#{hp:={single, HA}}, id:=_I1}, #{state:=#{hp:={single, HB}}, id:=_I2}, Log) when HA < 0 orelse HB < 0 ->
    erlang:display({ended, someone_died}),
    {log, lists:reverse(Log)};
loop(_, #{selected_skills:=[], id:=_I1}, #{selected_skills:=[], id:=_I2}, Log)->
    erlang:display({ended, no_skills}),
    {log, lists:reverse(Log)};


% ------------------------- LOOP FOR CAST -----------------------------

loop(State, A, B, L) ->

    {AppliedA, AppliedB, AppliedLog} = apply_move_ordered(State, A, B, L),

    loop(next_state(State, A, B), AppliedA, AppliedB, AppliedLog).


start({A, B}) ->

    erlang:display(battle_begins),

    loop(next_state(#{seq=>0, stage=>casting}, A, B), A, B, []).
