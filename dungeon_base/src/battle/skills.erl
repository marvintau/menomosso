-module(skills).

-author('Yue Marvin Tao').

-export([init_table/0, create_skills/0]).

-export([update_skills/1]).

init_table() ->
    ets:new(skills, [set, public, named_table]),
    create_skills().

update_skills(Data) ->
    Res = skills_to_erlang:skills(Data),
    error_logger:info_report(Res),
    true = ets:delete_all_objects(skills),
    ets:insert(skills, Res),
    ok.

physical_attack_spec(Absorbable) ->
    {physical, attack, Absorbable, non_resistable, 0}.
physical_attack_spec() ->
    {physical, attack, non_absorbable, non_resistable, 0}.

magic_cast_spec(Resis) ->
    {magic, cast, non_absorbable, Resis, 0}.
magic_cast_spec() ->
    magic_cast_spec(non_resistable).

test_attr(Attr, Dest) ->
    {{add, {{single, 1}}, physical_attack_spec()}, {attr, attr, Attr, Dest}}.

die() ->
    {{add, {{single, -1300}}, physical_attack_spec()}, {attr, state, hp, def}}.

plain_attack() ->
    {{add, {{attr, attr, atk_range, off}}, physical_attack_spec()}, {attr, state, hp, def}}.

counter_attack(Times) ->
    {{add_inc_mul, {{attr, state, diff, off}, {single, Times}}, physical_attack_spec()}, {attr, state, hp, def}}.

buff(AttrType, Attr, Buff) ->
    {{add_mul, {{single, 1+Buff}}, magic_cast_spec()}, {attr, AttrType, Attr, off}}.

seq() ->
    seq(0).
seq(LastFor) ->
    seq(LastFor, casting).
seq(LastFor, Stage) ->
    seq(LastFor, Stage, []).
seq(LastFor, Stage, Conds) ->
    {{seq_norm, 0, LastFor, Stage}, Conds}.

next_damage() ->
    next_damage(1).
next_damage(LastFor) ->
    {{next_cast_norm, LastFor, {physical, attack, none, none}, casting}, []}.

opponent_critical() ->
    {attack, '==', {attr, attr, outcome, def}}.


create_skills() ->
    true = ets:delete_all_objects(skills),

    Skills = [
        {single_attack, [{0, [
            {seq(), [test_attr(armor, def), test_attr(agility, def), test_attr(hit, def), test_attr(block, def), test_attr(dodge, def), test_attr(resist, def), test_attr(critical, def), die()]}
        ]}]},

        {double_attack, [{0, [
            {seq(), [plain_attack(), plain_attack()]}
        ]}]},

        {triple_attack, [{0, [
            {seq(), [plain_attack(), plain_attack(), plain_attack()]}
        ]}]},

        {charm_of_foresight, [{0, [
            {next_damage(), [buff(attr, dodge, 0.25), buff(attr, block, 0.25)]}
        ]}]},
        {fortify_armor, [{0, [
            {next_damage(), [buff(attr, armor, 0.3)]}
        ]}]},
        {increase_crit, [{0, [
            {next_damage(), [buff(attr, critical, 0.25)]}
        ]}]},

        {counterattack, [{0, [
            {seq(2, counter, [opponent_critical()]), [counter_attack(3)]}
        ]}]},

        {healing_potion, [{0, [
            {seq(), [{{add, {{range, 175, 255}}, magic_cast_spec()}, {attr, state, hp, off}}]}
        ]}]},

        {poison_gas, [{0.5, [
            {seq(1), [{{set, {{single, is_stunned}}, magic_cast_spec(resistable)}, {attr, state, hp, def}}]}
        ]},{0.5,
            {seq(1), [{{set, {{single, is_stunned}}, magic_cast_spec(resistable)}, {attr, state, hp, off}}]}
        }]},

        {rune_of_the_void, [{0, [
            {seq(), [{{set, {{single, cast_disabled}}, magic_cast_spec()}, {attr, attr, cast_disabled, def}}]}
        ]}]},

        {holy_hand_grenade, [{0, [
            {seq(), [{{add, {{range, -500, -1}}, magic_cast_spec(resistable)}, {attr, state, hp, def}}]}
        ]}]},

        {talisman_of_death, [{0, [
            {seq(), [{{add_mul, {{single, -0.15}}, plain_attack()}, {attr, state, hp, def}}]}
        ]}]},

        {talisman_of_spellshrouding, [{0, [
            {seq(), [buff(attr, resist, 1)]}
        ]}]},

        {sure_hit, [{0, [
            {seq(1), [
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, resist, def}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, block, def}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, dodge, def}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, critical, off}}
            ]}
        ]}]},

        {concussion, [{0, [
            {seq(0), [
                {{add_inc_mul, {{attr, attr, agility, def}, {single, -1}}, physical_attack_spec(absorbable)}, {attr, state, hp, def}},
                {{add_inc_mul, {{attr, attr, critical, def}, {single, -0.5}}, physical_attack_spec()}, {attr, state, hp, def}}
            ]}
        ]}]},

        {double_swing, [{0, [
            {seq(2, counter, []), [plain_attack(), plain_attack()]}
        ]}]},

        {first_aid, [{0, [
            {seq(), [
                {{add_inc_mul, {{attr, state, hp, off}, {single, 0.08}}, magic_cast_spec()}, {attr, state, hp, off}}
            ]}
        ]}]},

        {shield_wall, [{0, [
            {seq(1), [
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, dodge, off}},
                {{set, {{single, 120}}, magic_cast_spec()}, {attr, attr, block, off}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, hit_bonus, def}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, critical, def}}
            ]}
        ]}]}
    ],

    ets:insert(skills, Skills),
    ok.
