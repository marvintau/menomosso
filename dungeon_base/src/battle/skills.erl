-module(skills).

-author('Yue Marvin Tao').

-export([init_table/0, create_skills/0]).

-export([update_skills/1]).

init_table() ->
    ets:new(skills, [set, public, named_table, {read_concurrency, true}]),
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
    {physical, attack, absorbable, non_resistable, 0}.

physical_cast_spec() ->
    {physical, {cast, normal}, absorbable, non_resistable, 0}.

magic_cast_spec(Resis) ->
    {magic, {cast, normal}, non_absorbable, Resis, 0}.
magic_cast_spec() ->
    magic_cast_spec(non_resistable).

plain_attack() ->
    {{add, {{attr, attr, atk_range, off}}, physical_attack_spec()}, {attr, state, hp, def}, {chase, blowable}}.
plain_attack_no_blow() ->
    {{add, {{attr, attr, atk_range, off}}, physical_attack_spec()}, {attr, state, hp, def}, {chase, non_blowable}}.
stand_plain_attack() ->
    {{add, {{attr, attr, atk_range, off}}, physical_cast_spec()}, {attr, state, hp, def}, {chase, non_blowable}}.

counter_attack(Times) ->
    {{add_inc_mul, {{attr, state, diff, off}, {single, Times}}, physical_attack_spec()}, {attr, state, hp, def}, {stand, blowable}}.

buff(AttrType, Attr, Buff) ->
    {{add_mul, {{single, 1+Buff}}, magic_cast_spec()}, {attr, AttrType, Attr, off}, {stand, non_blowable}}.

toggle(Who, AttrType, Attr, Switch) ->
    {{set, {{single, Switch}}, magic_cast_spec()}, {attr, AttrType, Attr, Who}, {stand, non_blowable}}.

seq() ->
    seq(0).
seq(LastFor) ->
    seq(LastFor, casting).
seq(LastFor, Stage) ->
    seq(LastFor, Stage, []).
seq(LastFor, Stage, Conds) ->
    {{seq_norm, 0, LastFor, Stage}, Conds}.

next_damage() ->
    next_damage(0).
next_damage(LastFor) ->
    next_damage(LastFor, []).
next_damage(LastFor, Conds) ->
    next_damage(LastFor, Conds, settling).
next_damage(LastFor, Conds, Stage) ->
    {{next_defense_norm, LastFor, {physical, attack, none, none}, Stage}, Conds}.

next_attack() ->
    next_attack(1).
next_attack(LastFor) ->
    {{next_offense_norm, LastFor, {physical, attack, none, none}, settling}, []}.


opponent_critical() ->
    {critical, '==', {attr, attr, outcome, def}}.


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
            {seq(), [plain_attack_no_blow(), plain_attack_no_blow(), plain_attack_no_blow()]}
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
            {next_damage(0, [opponent_critical()], counter), [counter_attack(3)]}
        ]}]},

        {healing_potion, [{0, [
            {seq(), [{{add, {{range, 175, 225}}, magic_cast_spec()}, {attr, state, hp, off}, {back, non_blowable}}]}
        ]}]},

        {pierce_armor, [{0, [
            {seq(3),[{{add_mul, {{single, -0.5}}, magic_cast_spec()}, {attr, attr, armor, def}, {stand, non_blowable}}]}
        ]}]},

        {poison_gas, [{0.5, [
            {seq(2), [
                toggle(def, attr, is_stunned, 1),
                toggle(def, attr, cast_disabled, 1),
                toggle(def, attr, block, 0),
                toggle(def, attr, dodge, 0),
                toggle(def, attr, resist, 0)
            ]}
        ]},{0.5,[
            {seq(2), [
                toggle(off, attr, is_stunned, 1),
                toggle(off, attr, cast_disabled, 1),
                toggle(off, attr, block, 0),
                toggle(off, attr, dodge, 0),
                toggle(off, attr, resist, 0)
            ]}
        ]}]},

        {rune_of_the_void, [{0, [
            {seq(), [{{set, {{single, 1}}, magic_cast_spec()}, {attr, attr, cast_disabled, def}, {stand, non_blowable}}]}
        ]}]},

        {holy_hand_grenade, [{0, [
            {seq(), [{{add, {{range, -500, -1}}, magic_cast_spec(resistable)}, {attr, state, hp, def}, {back, non_blowable}}]}
        ]}]},

        {talisman_of_shielding, [{0, [
            {next_damage(), [buff(attr, armor, 0.5)]}
        ]}]},

        {talisman_of_death, [{0, [
            {seq(), [{{add_mul, {{single, -0.15}}, magic_cast_spec()}, {attr, state, hp, def}, {back, non_blowable}}]}
        ]}]},

        {talisman_of_spellshrouding, [{0, [
            {next_damage(), [buff(attr, resist, 1)]}
        ]}]},

        {sure_hit, [{0, [
            {next_attack(), [
                toggle(def, attr, resist, 0),
                toggle(def, attr, block, 0),
                toggle(def, attr, dodge, 0),
                toggle(def, attr, critical, 0),
                toggle(def, attr, hit, 0)
            ]}
        ]}]},

        {concussion, [{0, [
            {seq(0), [
                {{add_inc_mul, {{attr, attr, critical, def}, {single, -0.5}}, magic_cast_spec()}, {attr, state, hp, def}, {stand, non_blowable}},
                {{add_inc_mul, {{attr, attr, agility, def}, {single, -1}}, physical_attack_spec(absorbable)}, {attr, state, hp, def}, {stand, non_blowable}}
            ]}
        ]}]},

        {double_swing, [{0, [
            {seq(2, counter, []), [stand_plain_attack(), plain_attack()]}
        ]}]},

        {first_aid, [{0, [
            {seq(), [
                {{add_inc_mul, {{attr, state, hp, off}, {single, 0.08}}, magic_cast_spec()}, {attr, state, hp, off}, {back, non_blowable}}
            ]}
        ]}]},

        {shield_wall, [{0, [
            {next_damage(), [
                toggle(off, attr, dodge, 0),
                toggle(off, attr, block, 120),
                toggle(def, attr, hit, 0),
                toggle(def, attr, critical, 0)
            ]}
        ]}]},

        {chain_lock, [{0, [
            {seq(), [
                toggle(def, attr, cast_disabled, 1)
            ]}
        ]}]},

        {critical_strike, [{0, [
            {next_attack(0), [
                toggle(def, attr, dodge, 0),
                toggle(def, attr, block, 0),
                toggle(off, attr, critical, 120),
                toggle(off, attr, hit, 0)
            ]}
        ]}]},

        {deadly_strike, [{0, [
            {seq(), [
                {{add_inc_mul, {{attr, attr, atk_range, off}, {single, 2.5}}, physical_attack_spec()}, {attr, state, hp, def}, {chase, blowable}}
            ]}
        ]}]},

        {shield_breaker, [{0, [
            {seq(4), [
            toggle(def, attr, block, 0)
            ]},
            {next_damage(), [
                toggle(off, attr, is_stunned, 1),
                toggle(off, attr, cast_disabled, 1),
                toggle(off, attr, dodge, 0),
                toggle(off, attr, block, 0),
                toggle(off, attr, critical, 0),
                toggle(off, attr, hit, 0)
            ]}
        ]}]},

        {unbalancing_strike, [{0, [
            {seq(4), [
                {{add_mul, {{single, -0.07}}, magic_cast_spec()}, {attr, attr, dodge, def}, {stand, non_blowable}},
                {{add_mul, {{single, -0.07}}, magic_cast_spec()}, {attr, attr, block, def}, {stand, non_blowable}},
                {{add_mul, {{single, -0.07}}, magic_cast_spec()}, {attr, attr, hit, def}, {stand, non_blowable}}
            ]}
        ]}]}
    ],

    ets:insert(skills, Skills),
    ok.
