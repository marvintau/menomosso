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
    {physical, {attack, normal}, Absorbable, non_resistable, 0}.
physical_attack_spec_no_blow() ->
    {physical, {attack, repose_no_blow}, non_absorbable, non_resistable, 0}.
physical_attack_spec() ->
    {physical, {attack, normal}, absorbable, non_resistable, 0}.

physical_cast_spec() ->
    {physical, {cast, normal}, non_absorbable, non_resistable, 0}.

magic_cast_spec(Resis) ->
    {magic, {cast, normal}, non_absorbable, Resis, 0}.
magic_cast_spec() ->
    magic_cast_spec(non_resistable).

test_attr(Attr, Dest) ->
    {{add, {{single, 1}}, magic_cast_spec()}, {attr, attr, Attr, Dest}}.

die() ->
    {{add, {{single, -1300}}, physical_attack_spec()}, {attr, state, hp, def}}.

plain_attack() ->
    {{add, {{attr, attr, atk_range, off}}, physical_attack_spec()}, {attr, state, hp, def}}.
plain_attack_no_blow() ->
    {{add, {{attr, attr, atk_range, off}}, physical_attack_spec_no_blow()}, {attr, state, hp, def}}.
stand_plain_attack() ->
    {{add, {{attr, attr, atk_range, off}}, physical_cast_spec()}, {attr, state, hp, def}}.

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
    {{next_defense_norm, LastFor, {physical, attack, none, none}, settling}, []}.

next_attack() ->
    next_attack(1).
next_attack(LastFor) ->
    {{next_offense_norm, LastFor, {physical, attack, none, none}, settling}, []}.


opponent_critical() ->
    {attack, '==', {attr, attr, outcome, def}}.


create_skills() ->
    true = ets:delete_all_objects(skills),

    Skills = [
        {single_attack, [{0, [
            {seq(), [plain_attack()]}
        ]}]},

        {double_attack, [{0, [
            {seq(), [plain_attack_no_blow(), plain_attack()]}
        ]}]},

        {triple_attack, [{0, [
            {seq(), [plain_attack_no_blow(), stand_plain_attack(), stand_plain_attack()]}
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
            {seq(), [{{add, {{range, 175, 225}}, magic_cast_spec()}, {attr, state, hp, off}}]}
        ]}]},

        {pierce_armor, [{0, [
            {seq(3),[{{add_mul, {{single, -0.5}}, magic_cast_spec()}, {attr, attr, armor, def}}]}
        ]}]},

        {poison_gas, [{0.5, [
            {seq(2), [
                {{set, {{single, 1}}, magic_cast_spec()}, {attr, attr, is_stunned, def}},
                {{set, {{single, 1}}, magic_cast_spec()}, {attr, attr, cast_disabled, def}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, block, def}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, dodge, def}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, resist, def}}
            ]}
        ]},{0.5,[
            {seq(2), [
                {{set, {{single, 1}}, magic_cast_spec()}, {attr, attr, is_stunned, off}},
                {{set, {{single, 1}}, magic_cast_spec()}, {attr, attr, cast_disabled, off}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, block, off}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, dodge, off}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, resist, off}}
            ]}
        ]}]},

        {rune_of_the_void, [{0, [
            {seq(), [{{set, {{single, 1}}, magic_cast_spec()}, {attr, attr, cast_disabled, def}}]}
        ]}]},

        {holy_hand_grenade, [{0, [
            {seq(), [{{add, {{range, -500, -1}}, magic_cast_spec(resistable)}, {attr, state, hp, def}}]}
        ]}]},

        {talisman_of_shielding, [{0, [
            {next_damage(), [buff(attr, armor, 0.5)]}
        ]}]},

        {talisman_of_death, [{0, [
            {seq(), [{{add_mul, {{single, -0.15}}, physical_attack_spec()}, {attr, state, hp, def}}]}
        ]}]},

        {talisman_of_spellshrouding, [{0, [
            {next_damage(), [buff(attr, resist, 1)]}
        ]}]},

        {sure_hit, [{0, [
            {next_attack(2), [
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, resist, def}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, block, def}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, dodge, def}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, critical, off}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, hit, off}}
            ]}
        ]}]},

        {concussion, [{0, [
            {seq(0), [
                {{add_inc_mul, {{attr, attr, critical, def}, {single, -0.5}}, magic_cast_spec()}, {attr, state, hp, def}},
                {{add_inc_mul, {{attr, attr, agility, def}, {single, -1}}, physical_attack_spec(absorbable)}, {attr, state, hp, def}}
            ]}
        ]}]},

        {double_swing, [{0, [
            {seq(2, counter, []), [stand_plain_attack(), plain_attack()]}
        ]}]},

        {first_aid, [{0, [
            {seq(), [
                {{add_inc_mul, {{attr, state, hp, off}, {single, 0.08}}, magic_cast_spec()}, {attr, state, hp, off}}
            ]}
        ]}]},

        {shield_wall, [{0, [
            {next_damage(), [
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, dodge, off}},
                {{set, {{single, 120}}, magic_cast_spec()}, {attr, attr, block, off}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, hit, def}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, critical, def}}
            ]}
        ]}]},

        {chain_lock, [{0, [
            {seq(), [
                {{set, {{single, 1}}, magic_cast_spec(resistable), {attr, attr, cast_disabled, def}}}
            ]}
        ]}]},

        {crtical_strike, [{0, [
            {next_attack(1), [
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, dodge, def}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, block, def}},
                {{set, {{single, 120}}, magic_cast_spec()}, {attr, attr, critical, off}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, hit, off}}
            ]}
        ]}]},

        {deadly_strike, [{0, [
            {seq(1), [
                {{set, {{single, 2.5}}, magic_cast_spec()}, {attr, attr, damage_multiplier, off}}
            ]}
        ]}]},

        {shield_breaker, [{0, [
            {seq(4), [
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, block, def}}
            ]},
            {next_damage(), [
                {{set, {{single, 1}}, magic_cast_spec()}, {attr, attr, is_stunned, off}},
                {{set, {{single, 1}}, magic_cast_spec()}, {attr, attr, cast_disabled, off}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, dodge, off}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, block, off}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, critical, off}},
                {{set, {{single, 0}}, magic_cast_spec()}, {attr, attr, hit, off}}
            ]}
        ]}]},

        {unbalancing_strike, [{0, [
            {seq(4), [
                {{add_mul, {{single, -0.07}}, magic_cast_spec()}, {attr, attr, dodge, def}},
                {{add_mul, {{single, -0.07}}, magic_cast_spec()}, {attr, attr, block, def}},
                {{add_mul, {{single, -0.07}}, magic_cast_spec()}, {attr, attr, hit, def}}
            ]}
        ]}]}
    ],

    ets:insert(skills, Skills),
    ok.
