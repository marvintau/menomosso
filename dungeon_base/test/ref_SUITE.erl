-module(ref_SUITE).

-author('Yue Marvin Tao').

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0]).
-export([init_per_group/2, end_per_group/2]).

-export([ref_test/1, trans_test/1, battle/1]).

all() ->
    [{group, ref_group}].

groups() ->
    [{ref_group, [{repeat, 1}], [ref_test, trans_test, battle]}].

init_per_group(ref_group, Config) ->

    Args = [{host, "localhost"}, {username, "yuetao"}, {password, "asdsadasd"}, {database, "dungeon"}, {timeout, 100}],
    dungeon_base:start(Args),
    ref_test_server:start({}),
    Config.

end_per_group(ref_group, _Config)->
    ref_test_server:stop(),
    dungeon_base:stop().

ref_test(_Config) ->

    RefDefHP = {attr, state, hp, def},

    42 = ref_test_server:val({single, 42}),
    Res = ref_test_server:val({attr, attr, atk_range, off}),
    % erlang:display({random_attack_damage, Res}),
    2700 = ref_test_server:val(RefDefHP),
    ref_test_server:set(RefDefHP, 2800),
    ok.

output_trans([]) -> ok;
output_trans([Ops | Rem]) ->

    RefDefHP = {attr, state, hp, def},
    RefOffHP = {attr, state, hp, off},
    {#{attr:=#{outcome:=Outcome2}, state:=#{hp:=HPO2}}, #{state:=#{hp:=HP2}=State}} = ref_test_server:trans(Ops, RefDefHP),
    % erlang:display(''),
    % erlang:display({Ops, outcome, Outcome2, state, State}),
    output_trans(Rem).


trans_test(_Config) ->

    RefDefHP = {attr, state, hp, def},
    RefOffHP = {attr, state, hp, off},

    PhysAtkNonResAbs = {physical, attack, non_resistable, absorbable, 0},
    MagicAtkNonResNonAbs = {magic, attack, non_resistable, non_absorbable, 0},
    MagicSkillResNonAbs = {magic, cast, resistable, non_absorbable, 0},
    MagicSkillResAbs = {magic, cast, resistable, absorbable, 0.5},

    Op = {{attr, attr, armor, def}, {single, -0.1}},

    RefDefHP = {attr, state, hp, def},
    RefOffHP = {attr, state, hp, off},

    AttrType = [physical, magic],
    MoveType = [attack, cast],
    Absorbable = [absorbable, non_absorbable],
    Resistable = [resistable, non_resistable],
    Failure = [0.5, 0],

    AttackSpecs = [{Attr, Move, Abs, Res, Fail} || Attr <- AttrType, Move <- MoveType, Abs <- Absorbable, Res <- Resistable, Fail <- Failure],

    Instructs = [{add, {{single, -20}}}, {add_mul, {{single, -0.01}}}, {add_inc_mul, {{attr, attr, armor, def}, {single, -0.1}}}],

    Ops = [{Inst, RefOps, AttackSpec} || {Inst, RefOps} <- Instructs, AttackSpec <- AttackSpecs],

    output_trans(Ops),

    ok.

random_seq(Sample) ->
    random_seq(Sample, 10).
random_seq(Sample, Length) ->
    [ lists:nth(random:uniform(length(Sample)), Sample) || X <- lists:seq(1, Length)].

battle(_Config) ->

    {Off, Def} = ref_test_server:get(),

    PlainAttackOff = Off#{selected_skills:=random_seq([single_attack, double_attack, triple_attack])},
    PlainAttackDef = Def#{selected_skills:=random_seq([single_attack, double_attack, triple_attack])},

    CounterAttackDef = Def#{selected_skills:= [counter_back| random_seq([single_attack, double_attack, triple_attack], 9)]},

    erlang:display(ref_test_server:battle(PlainAttackOff, CounterAttackDef)).
