
-module(effect).

-author('Yue Marvin Tao').

-export([apply/4]).

log(EffName, Mover, #{seq:=Seq, stage:=Stage}, #{state:=#{hp:=HpO, position:=PosO}}, #{state:=#{hp:=HpD, position:=PosD}}, Logs) ->

    {[
        { seq, Seq }, {stage, Stage}, { offender, Mover },
        { action, EffName},
        { effects, Logs }, { damage, 0 },
        { offenderHP, HpO },
        { defenderHP, HpD },
        { offenderPos, PosO},
        { defenderPos, PosD},
        { offenderPosAct, none}, {defenderPosAct, none}
    ]}.

   


% ============================ EFFECT ===========================================
% for each stage, effect processes the whole effect table, get the final player
% context and log.

apply(S, #{effects:=Effects}=O, D, Log) ->

    apply(S, O, D, Log, Effects).

apply(_S, #{hp:=H1}=O, #{hp:=H2}=D, Log, _) when (H1 =< 0) or (H2 =< 0) ->
    {O, D, Log};
    
apply(_S, O, D, Log, []) ->
    {O, D, Log};

apply(#{seq:=Seq}=S, O, D, Log, [ {Name, Mover, Conds, Transes} | Remaining]) ->


    {NewO, NewD, NewLog} = case conds:check(Conds, S, O, D) of
        
        true ->
            {NextO, NextD, Logs} = trans:apply(Conds, Transes, O, D),
            {NextO, NextD, [log(Name, Mover, S, NextO, NextD, Logs)]};
        
        _    ->
            {O, D, []}
    end,

    case (Name == freeze) and (Seq == 1) of
    true ->
        error_logger:info_report(NewD);
    _ -> ok
    end,

    apply(S, NewO, NewD, lists:append(NewLog, Log), Remaining).

