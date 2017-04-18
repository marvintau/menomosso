-module(conds).

-export([seq/3, check/4]).

rand() -> element(3, erlang:timestamp())/1000000.

% seq把在技能描述里关于“从放技能后的第几回合开始”和“持续几回合”，翻译成一场战斗中
% 实际的回合序号列表。在检查的时候只看当前回合序号是否存在于回合序号列表内
seq({{seq_rand, Start, {Last1, Last2}, Phase}, Others}, CurrSeq, _Skills) ->
    {{lists:seq(CurrSeq + Start, rand() * (Last2 - Last1) + Last1), Phase}, Others};

seq({{seq_ever, Start, null, Phase}, Others}, CurrSeq, _Skills) ->
    {{lists:seq(CurrSeq + Start, 20), Phase}, Others};

seq({{seq_norm, Start, Last, Phase}, Others}, CurrSeq, _Skills) ->
    {{lists:seq(CurrSeq + Start, CurrSeq + Start + Last), Phase}, Others};

seq({{Next, Last, {Attr, {Move, _}, Abs, Res}, Phase}, Others}, CurrSeq, Skills) when (Next == next_cast_norm) or (Next == next_defense_norm) ->

    erlang:display(here),

    IsPatternMatches = fun({{_Op, _Operand, {AttrG, MoveG, AbsG, ResG}}, _}) ->
        ((AttrG == Attr) or (Attr == none)) and ((MoveG == Move) or (Move == none)) and
        ((AbsG == Abs) or (Abs == none)) and ((ResG == Res) or (Res == none)) end,

    CondPerSkill = [ {Index, element(2, hd(ets:lookup(skills, SkillName)))} || {SkillName, Index} <- lists:zip(Skills, lists:seq(1, length(Skills)))],
    CondPerEffectGroup = lists:flatten([ [ {Seq, EffGroup} || {_Prob, EffGroup} <- EffectGroups ] || {Seq, EffectGroups} <- CondPerSkill]),
    CondPerCond = lists:flatten([ [{Seq, Eff} || {_EffSeqCond, Eff} <- EffectGroup] || {Seq, EffectGroup} <- CondPerEffectGroup]),
    CondPerEff = [ CurrSeq + Seq - 1 || {Seq, X} <- [ {Seq, lists:any(IsPatternMatches, TransList)} || {Seq, TransList} <- CondPerCond], X == true],

    {{lists:sublist(CondPerEff, Last), Phase}, Others}.


% 用于比较的算符
comp({Val, '==', TAP}, O, D) ->
    Val == ref:val(TAP, O, D);

comp({Val, '>', TAP}, O, D) ->
    Val > ref:val(TAP, O, D);

comp({Val, '<', TAP}, O, D) ->
    Val < ref:val(TAP, O, D).



comps(CondList, O, D) ->
	comps(CondList, O, D, true).

comps([Cond | RemConds], O, D, TrueValue) ->
    comps(RemConds, O, D, TrueValue and comp(Cond, O, D));

comps([], _, _, TrueValue) ->
	TrueValue.


% ====================== SEQUENTIAL CONDITION CHECK =============================
% checks whether the battle goes to specific round and stage.
seq_check({SeqList, Stage}, #{seq:=CurrSeq, stage:=CurrStage}, #{attr:=#{cast_disabled:={single, CastDisabled}}}) ->
    lists:any(fun(Seq) -> CurrSeq == Seq end, SeqList) and (Stage == CurrStage) and not ((Stage == casting) and (CastDisabled /= 0)).

check({SeqCond, CondList}, S, O, D) ->
    seq_check(SeqCond, S, O) and comps(CondList, O, D).
