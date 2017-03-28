-module(battle).

-author('Yue Marvin Tao').

-export([start/1]).

rand() ->
    element(3, erlang:timestamp())/1000000.

% ------------- HELPER FUNCTION FOR CHOOSING NEW OFFENDER --------------

toss(#{casts:=[rune_of_the_void|_], id:=A}, _) -> A;
toss(_, #{casts:=[rune_of_the_void|_], id:=B}) -> B;

toss(#{id:=A, attr:=#{agility:=AgiA}},
     #{id:=B, attr:=#{agility:=AgiB}}) ->
    case rand() * (AgiA + AgiB) > AgiA of
        true -> B;
        _    -> A
    end.


refresh_attributes(#{orig_attr := OrigA} = PlayerA, #{orig_attr := OrigB} = PlayerB) ->
    {PlayerA#{attr:=OrigA}, PlayerB#{attr:=OrigB}}.


next_state(#{stage:=casting, seq:=Seq}=S, A, B) ->
    erlang:display({"=============", new_round, Seq+1}),
    S#{stage:=settling, seq:=Seq+1, offender:=toss(A, B)};

next_state(#{stage:=settling}=S, _, _) ->
    erlang:display({"=============", begin_casting}),
    S#{stage:=casting}.


apply_move_both(#{stage:=settling}=S, A, B, L) ->
    {OpA, OpB, OpLog} = effect:apply(S, A, B, L),
    {Op2B, Op2A, Op2Log} = effect:apply(S, OpB, OpA, OpLog),
    {Op2A, Op2B, Op2Log};

apply_move_both(#{stage:=casting}=S, A, B, L) ->
    {OpA, OpB, OpLog} = cast:apply(S, A, B, L),
    {Op2B, Op2A, Op2Log} = cast:apply(S, OpB, OpA, OpLog),
    {RefreshedA, RefreshedB} = refresh_attributes(Op2A, Op2B),
    {RefreshedA, RefreshedB, Op2Log}.


apply_move_ordered(#{offender:=Off}=S, #{id:=Off}=A, B, L) ->
    apply_move_both(S, A, B, L);

apply_move_ordered(#{offender:=Off}=S, A, #{id:=Off}=B, L) ->
    {NewB, NewA, NewL} = apply_move_both(S, B, A, L),
    {NewA, NewB, NewL}.


% ======================= MAIN BATTLE LOOP ============================


% ------------------------- TERMINATION ------------------------------
% The condition of terminating is one competitor's HP falls below zero.
% When exiting the main loop, the log will be reversed to it's natural
% order.

loop(_, #{state:=#{hp:=HP1}, id:=I1}, #{state:=#{hp:=HP2}, id:=I2}, Log) when HP1 < 0 orelse HP2 < 0 ->

    Winner = if HP1 < 0 -> I2;
                HP2 < 0 -> I1
             end,

    {done, 
        {records, lists:reverse([L || L <- Log, L =/= {[]}])}, {winner,Winner}
    };


% ------------------------- LOOP FOR CAST -----------------------------

loop(State, A, B, L) ->

    {AppliedA, AppliedB, AppliedLog} = apply_move_ordered(State, A, B, L),

    loop(next_state(State, A, B), AppliedA, AppliedB, AppliedLog).


start({P1, P2}) ->

    erlang:display(battle_begins),

    loop(#{seq=>0, stage=>casting}, P1, P2, []).

