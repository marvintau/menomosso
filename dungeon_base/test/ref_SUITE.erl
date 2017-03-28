-module(ref_SUITE).

-author('Yue Marvin Tao').

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0]).
-export([init_per_group/2, end_per_group/2]).

-export([ref_test/1, trans_test/1]).

all() ->
    [{group, ref_group}].

groups() ->
    [{ref_group, [{repeat, 1}], [ref_test, trans_test]}].

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
    75 = ref_test_server:val({attr, attr, agility, off}),
    2700 = ref_test_server:val(RefDefHP),
    ref_test_server:set(RefDefHP, 2800),
    ok.

trans_test(_Config) ->

    RefDefHP = {attr, state, hp, def},
    RefOffHP = {attr, state, hp, off},

    PhysAtkNonResAbs = {physical, attack, non_resistable, absorbable, 0},
    MagicAtkNonResNonAbs = {magic, attack, non_resistable, non_absorbable, 0},
    MagicCastResNonAbs = {magic, cast, resistable, non_absorbable, 0},
    MagicCastResAbs = {magic, cast, resistable, absorbable, 0.5},

    Op = {{attr, attr, armor, def}, {single, -0.1}},

    ct:print("~ts~n", ["basic trans operations"]),

    {#{attr:=#{outcome:=Outcome2}, state:=#{hp:=HPO2}}, #{state:=#{hp:=HP2}}} = ref_test_server:trans({add, {{single, -20}}, PhysAtkNonResAbs}, RefDefHP),
    erlang:display({outcomeO, Outcome2, hp, HPO2}),
    {#{attr:=#{outcome:=Outcome3}, state:=#{hp:=HPO3}}, #{state:=#{hp:=HP3}}} = ref_test_server:trans({add_mul, {{single, -0.01}}, PhysAtkNonResAbs}, RefDefHP),
    erlang:display({outcomeO, Outcome3, hp, HPO3}),
    {#{attr:=#{outcome:=Outcome4}, state:=#{hp:=HPO4}}, #{state:=#{hp:=HP4}}} = ref_test_server:trans({add_inc_mul, Op, PhysAtkNonResAbs}, RefDefHP),
    erlang:display({outcomeO, Outcome4, hp, HPO4}),

    ct:print("~ts~n", ["roulette outcomes magic"]),
    {#{attr:=#{outcome:=Outcome5}, state:=#{hp:=HPO5}}, #{state:=#{hp:=HP5}}} = ref_test_server:trans({add_inc_mul, Op, MagicAtkNonResNonAbs}, RefDefHP),
    erlang:display({outcomeO, Outcome5, hp, HPO5}),
    {#{attr:=#{outcome:=Outcome6}, state:=#{hp:=HPO6}}, #{state:=#{hp:=HP6}}} = ref_test_server:trans({add_inc_mul, Op, MagicCastResNonAbs}, RefDefHP),
    erlang:display({outcomeO, Outcome6, hp, HPO6}),

    ct:print("~ts~n", ["roulette outcomes physical"]),
    {#{attr:=#{outcome:=Outcome7}, state:=#{hp:=HPO7}}, #{state:=#{hp:=HP7}}} = ref_test_server:trans({add_inc_mul, Op, PhysAtkNonResAbs}, RefDefHP),
    erlang:display({outcomeO, Outcome7, hp, HPO7}),

    ct:print("~ts~n", ["cast failure"]),
    {#{attr:=#{outcome:=Outcome8}, state:=#{hp:=HPO8}}, #{state:=#{hp:=HP8}}} = ref_test_server:trans({add_inc_mul, Op, MagicCastResAbs}, RefDefHP),
    erlang:display({outcomeO, Outcome8, hp, HPO8}),
    ok.
