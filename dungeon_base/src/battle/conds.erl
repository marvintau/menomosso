-module(conds).

-export([seq/3, check/4]).

rand() -> element(3, erlang:timestamp())/1000000.


% seq把在技能描述里关于“从放技能后的第几回合开始”和“持续几回合”，翻译成一场战斗中
% 实际的回合序号
seq({{seq_rand, Start, {Last1, Last2}, Phase}, Others}, CurrSeq, _Skills) ->
    {{lists:seq(CurrSeq + Start, rand() * (Last2 - Last1) + Last1), Phase}, Others};

seq({{seq_ever, Start, null, Phase}, Others}, CurrSeq, _Skills) ->
    {{lists:seq(CurrSeq + Start, 20), Phase}, Others};

seq({{seq_norm, Start, Last, Phase}, Others}, CurrSeq, _Skills) ->
    {{lists:seq(CurrSeq + Start, CurrSeq + Start + Last), Phase}, Others};

seq({{next_cast_norm, Last, GivenAttackSpec, Phase}, Others}, CurrSeq, Skills) ->

    IsPatternMatches = fun({AttrG, MoveG, AbsG, ResG}, {Attr, Move, Abs, Res}) ->
        ((AttrG == Attr) or (AttrG == none)) and ((MoveG == Move) or (MoveG == none)) and
        ((AbsG == Abs) or (AbsG == none)) and ((ResG == Res) or (ResG == none)) end,

    ActualSeqs = [ Index || {SkillName, Index} <- lists:zip(Skills, lists:seq(1, length(Skills))), Index > CurrSeq],
    {{lists:sublist(lists:filter(IsPatternMatches, ActualSeqs), Last), Phase}, Others}.


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
seq_check({SeqList, Stage}, #{seq:=CurrSeq, stage:=CurrStage}) ->
    erlang:display(SeqList),
    lists:any(fun(Seq) -> CurrSeq == Seq end, SeqList) and (Stage == CurrStage).

check({SeqCond, CondList}, S, O, D) ->
    seq_check(SeqCond, S) and comps(CondList, O, D).
