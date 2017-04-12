-module(cast).

-author('Yue Marvin Tao').

-export([parse/2, seq/3, get_effects/1, get_casts/1]).


% wrap all the operations. A mapping from original description of an effect
% along with the current state, to a final form of effect description. The
% latter function is the actual entrance that takes cast name as argument, and
% find the specification in database, and re-interpret it with battle context.

parse(single_trans, {Index, Name, {Cond, TransList} = EffectSpec, IsSuccessful}) ->
    [{Index, Name, Cond, Trans, IsSuccessful} || Trans <- TransList];

parse(trans_list, {Index, Name, {Prob, EffectSpecs}}) ->
    IsSuccessful = rand:uniform() > Prob,
    lists:concat([parse(single_trans, {Index, Name, EffectSpec, IsSuccessful}) || EffectSpec <- EffectSpecs]);

parse(cast, {Index, SkillName}) ->
    {Name, Groups} = hd(ets:lookup(skills, SkillName)),
    lists:concat([parse(trans_list, {Index, Name, Group}) || Group <- Groups]);

parse(list, {SkillList}) ->
    lists:concat([parse(cast, {Index, Skill}) || {Skill, Index} <- lists:zip(SkillList, lists:seq(1, length(SkillList))), Skill /= none]).


% seq把在技能描述里关于“从放技能后的第几回合开始”和“持续几回合”，翻译成一场战斗中
% 实际的回合序号列表。在检查的时候只看当前回合序号是否存在于回合序号列表内

seq({{seq_rand, Start, {Last1, Last2}, Phase}, Others}, CurrSeq, _Effects) ->
    {{lists:seq(CurrSeq + Start, rand:uniform() * (Last2 - Last1) + Last1), Phase}, Others};

seq({{seq_ever, Start, null, Phase}, Others}, CurrSeq, _Effects) ->
    {{lists:seq(CurrSeq + Start, 20), Phase}, Others};

seq({{seq_norm, Start, Last, Phase}, Others}, CurrSeq, _Effects) ->
    {{lists:seq(CurrSeq + Start, CurrSeq + Start + Last), Phase}, Others};

seq({{next_cast_norm, Last, {Attr, Move, Abs, Res}, Phase}, Others}, CurrSeq, Effects) ->

    CheckPatternMatch = fun({{_Op, _Operand, {AttrG, MoveG, AbsG, ResG, _}}, _}) ->
        ((AttrG == Attr) or (Attr == none)) and ((MoveG == Move) or (Move == none)) and
        ((AbsG == Abs) or (Abs == none)) and ((ResG == Res) or (Res == none)) end,

    CheckedIndex = [ {Index, CheckPatternMatch(Eff)} || {Index, _, _, Eff} <-Effects],
    FilteredIndex = [ I || {I, T} <- CheckedIndex, T == true, CurrSeq < I, CurrSeq + Last + 1 >= I],

    {{FilteredIndex, Phase}, Others}.

get_effects(Skills) ->
    Effects = parse(list, {Skills}),
    CondCheckedEffects = [{Index, Name, seq(Cond, Index, Effects), Trans, IsSuccessful} || {Index, Name, Cond, Trans, IsSuccessful} <- Effects],
    CondCheckedEffects.

compress([])->
    [];
compress(L)->
    compress(L,[]).

compress([H|[]],[H1|T1]) when H == H1 ->
    lists:reverse([H1|T1]);

compress([H|[]],Acc) ->
    lists:reverse([H|Acc]);

compress([H|T],[H1|T1]) when H == H1 ->
    compress(T,[H1|T1]);
compress([H|T],Acc) ->
    compress(T,[H|Acc]).

get_casts(Effects) ->
    List = [{Index, SkillName, IsSuccessful} || {Index, SkillName, _, _, IsSuccessful} <- Effects],
    compress(List).
