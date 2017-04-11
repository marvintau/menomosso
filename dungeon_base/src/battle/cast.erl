-module(cast).

-author('Yue Marvin Tao').

-export([cast/4, effect/4]).


rand() -> element(3, erlang:timestamp())/1000000.

% wrap all the operations. A mapping from original description of an effect
% along with the current state, to a final form of effect description. The
% latter function is the actual entrance that takes cast name as argument, and
% find the specification in database, and re-interpret it with battle context.

parse_single_effect(Name, {Cond, Trans} = EffectSpec, #{seq:=CurrSeq, offender:=Offender}, SelectedSkills) ->
    {Name, Offender, conds:seq(Cond, CurrSeq, SelectedSkills), Trans}.

parse_single_group(Name, {Prob, EffectSpecs}, S, SelectedSkills) ->
    case rand() > Prob of
        true ->
            [parse_single_effect(Name, EffectSpec, S, SelectedSkills) || EffectSpec <- EffectSpecs];
        _ ->
            []
    end.

parse_cast({Name, Groups}, S, SelectedSkills) ->
   lists:concat([parse_single_group(Name, Group, S, SelectedSkills) || Group <- Groups]).



cast(_S, #{attr:=#{cast_disabled:={single, cast_disabled}}}=O, D, L) -> {O, D, L};

cast(_S, #{selected_skills:=[]}=O, D, L) -> {O, D, L};

cast(_S, #{state:=#{hp:={single, HPO}}}=O, #{state:=#{hp:={single, HPD}}}=D, L) when (HPO < 0) or (HPD < 0) -> {O, D, L};

cast(_S, #{selected_skills:=[none | RemainingSkills]}=O, D, L) ->
    {O#{selected_skills:=RemainingSkills}, D, L};

cast(S, #{id:=IDO, player_name:=PlayerNameO, selected_skills:=[SkillName | RemainingSkills]=SelectedSkills, effects:=ExistingEffects}=O, D, L) ->

    CurrEffects = parse_cast(hd(ets:lookup(skills, SkillName)), S, SelectedSkills),
    NewEffects = lists:append([ExistingEffects,CurrEffects]),

    {O#{selected_skills:=RemainingSkills, effects:=NewEffects}, D, L}.


effect(S, #{effects:=Effects}=O, D, Log) ->
    effect(S, O, D, Log, Effects).

effect(_S, #{state:=#{hp:={single, H1}}}=O, #{state:=#{hp:={single, H2}}}=D, Log, _) when (H1 =< 0) or (H2 =< 0) ->
    {O, D, Log};

effect(_S, O, D, Log, []) ->
    {O, D, Log};

effect(#{seq:=Seq}=S, #{player_name:=PlayerName} = O, D, Log, [ {Name, Mover, Conds, Transes} | Remaining]) ->

    {NewO, NewD, NewLog} = case conds:check(Conds, S, O, D) of

        true ->
            trans:apply(S, Name, Transes, O, D);
        _    ->
            {O, D, []}
    end,

    effect(S, NewO, NewD, lists:append(NewLog, Log), Remaining).
