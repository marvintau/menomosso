-module(skills).

-author('Yue Marvin Tao').

-export([init_table/0, create_skills/0]).


init_table() ->
    ets:new(skills, [set, public, named_table, {read_concurrency, true}]),
    create_skills().

physical_attack_spec(Absorbable) ->
    {physical, attack, Absorbable, non_resistable, 0}.
physical_attack_spec() ->
    {physical, attack, absorbable, non_resistable, 0}.

physical_cast_spec() ->
    {physical, cast, absorbable, non_resistable, 0}.

magic_cast_spec(Resis) ->
    {magic, cast, non_absorbable, Resis, 0}.
magic_cast_spec() ->
    magic_cast_spec(non_resistable).

plain_attack() ->
    {{add_inc_mul, {{attr, atk_range, off}, {single, -1}}, physical_attack_spec()}, {attr, hp, def}, {chase, blowable}}.
plain_attack_no_blow() ->
    {{add_inc_mul, {{attr, atk_range, off}, {single, -1}}, physical_attack_spec()}, {attr, hp, def}, {chase, non_blowable}}.
stand_plain_attack() ->
    {{add_inc_mul, {{attr, atk_range, off}, {single, -1}}, physical_cast_spec()}, {attr, hp, def}, {stand, non_blowable}}.

counter_attack(Times) ->
    {{add_inc_mul, {{attr, diff, off}, {single, Times}}, physical_attack_spec()}, {attr, hp, def}, {stand, blowable}}.

buff(Attr, Buff) ->
    {{add_mul, {{single, 1+Buff}}, magic_cast_spec()}, {attr, Attr, off}, {stand, non_blowable}}.

toggle(Who, Attr, Switch) ->
    {{set, {{single, Switch}}, magic_cast_spec()}, {attr, Attr, Who}, {stand, non_blowable}}.

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
    {critical, '==', {attr, outcome, def}}.


modify_skill([{prob_group, WhichGroup} | Rem], ProbGroups) ->
    setelement(WhichGroup, modify_skill(Rem, element(WhichGroup, ProbGroups)), ProbGroups);
modify_skill([all_prob_group | Rem], ProbGroups) ->
    ProbGroupsList = tuple_to_list(ProbGroups),
    [modify_skill(Rem, ProbGroup) || ProbGroup <- ProbGroups];

modify_skill([{prob, NewProb}], ProbGroup) ->
    setelement(1, NewProb, ProbGroup);

modify_skill([{cond_group, WhichGroup} | Rem], ProbGroup) ->
    setelement(WhichGroup, modify_skill(Rem, element(WhichGroup, ProbGroup)), ProbGroup);

modify_skill([all_cond_group | Rem], ProbGroup) ->
    

create_skills() ->
    true = ets:delete_all_objects(skills),

    Skills = [
        {<<"single_attack">>, {{0, {
            {seq(), {plain_attack()}}
        }}}},

        {<<"double_attack">>, {{0, {
            {seq(), {stand_plain_attack(), plain_attack()}}
        }}}},

        {<<"triple_attack">>, {{0, {
            {seq(), {stand_plain_attack(), stand_plain_attack(), plain_attack_no_blow()}}
        }}}},

        {<<"charm_of_foresight">>, {{0, {
            {next_damage(), {buff(dodge, 0.25), buff(block, 0.25)}}
        }}}},
        {<<"fortify_armor">>, {{0, {
            {next_damage(), {buff(dodge, 0.3)}}
        }}}},
        {<<"increase_crit">>, {{0, {
            {next_damage(), {buff(critical, 0.25)}}
        }}}},

        {<<"counterattack">>, {{0, {
            {next_damage(0, {opponent_critical()}, counter), {counter_attack(3)}}
        }}}},

        {<<"healing_potion">>, {{0, {
            {seq(), {{{add, {{range, [175, 225]}}, magic_cast_spec()}, {attr, hp, off}, {back, non_blowable}}}}
        }}}},

        {<<"pierce_armor">>, {{0, [
            {seq(3), {{{add_mul, {{single, -0.5}}, magic_cast_spec()}, {attr, armor, def}, {stand, non_blowable}}}}
        ]}}},

        {<<"poison_gas">>, {{0.5, {
            {seq(2), {
                toggle(def, is_stunned, 1),
                toggle(def, cast_disabled, 1),
                toggle(def, block, 0),
                toggle(def, dodge, 0),
                toggle(def, resist, 0)
            }}
        }},{0.5,{
            {seq(2), {
                toggle(off, is_stunned, 1),
                toggle(off, cast_disabled, 1),
                toggle(off, block, 0),
                toggle(off, dodge, 0),
                toggle(off, resist, 0)
            }}
        }}}},

        {<<"rune_of_the_void">>, {{0, {
            {seq(), {{{set, {{single, 1}}, magic_cast_spec()}, {attr, cast_disabled, def}, {stand, non_blowable}}}}
        }}}},

        {<<"holy_hand_grenade">>, {{0, {
            {seq(), {{{add, {{range, [-500, -1]}}, magic_cast_spec(resistable)}, {attr, hp, def}, {back, non_blowable}}}}
        }}}},

        {<<"talisman_of_shielding">>, {{0, {
            {next_damage(), {buff(armor, 0.5)}}
        }}}},

        {<<"talisman_of_death">>, {{0, {
            {seq(), {{{add_mul, {{single, -0.15}}, magic_cast_spec()}, {attr, hp, def}, {back, non_blowable}}}}
        }}}},

        {<<"talisman_of_spellshrouding">>, {{0, {
            {next_damage(), {buff(resist, 1)}}
        }}}},

        {<<"sure_hit">>, {{0, {
            {next_attack(), {
                toggle(def, resist, 0),
                toggle(def, block, 0),
                toggle(def, dodge, 0),
                toggle(def, critical, 0),
                toggle(def, hit, 0)
            }}
        }}}},

        {<<"concussion">>, {{0, {
            {seq(0), {
                {{add_inc_mul, {{attr, critical, def}, {single, -0.5}}, magic_cast_spec()}, {attr, hp, def}, {stand, non_blowable}},
                {{add_inc_mul, {{attr, agility, def}, {single, -1}}, physical_attack_spec(absorbable)}, {attr, hp, def}, {stand, non_blowable}}
            }}
        }}}},

        {<<"double_swing">>, {{0, {
            {seq(2, counter, []), {stand_plain_attack(), plain_attack()}}
        }}}},

        {<<"first_aid">>, {{0, {
            {seq(), [
                {{add_inc_mul, {{attr, hp, off}, {single, 0.08}}, magic_cast_spec()}, {attr, hp, off}, {back, non_blowable}}
            ]}
        }}}},

        {<<"shield_wall">>, {{0, {
            {next_damage(), {
                toggle(off, dodge, 0),
                toggle(off, block, 120),
                toggle(def, hit, 0),
                toggle(def, critical, 0)
            }}
        }}}},

        {<<"chain_lock">>, {{0, {
            {seq(), {
                toggle(def, cast_disabled, 1)
            }}
        }}}},

        {<<"critical_strike">>, {{0, {
            {next_attack(0), {
                toggle(def, dodge, 0),
                toggle(def, block, 0),
                toggle(off, critical, 120),
                toggle(off, hit, 0)
            }}
        }}}},

        {<<"deadly_strike">>, {{0, {
            {seq(), {
                {{add_inc_mul, {{attr, atk_range, off}, {single, 2.5}}, physical_attack_spec()}, {attr, hp, def}, {chase, blowable}}
            }}
        }}}},

        {<<"shield_breaker">>, {{0, {
            {seq(4), {
                toggle(def, block, 0)
            }},
            {next_damage(), {
                toggle(off, is_stunned, 1),
                toggle(off, cast_disabled, 1),
                toggle(off, dodge, 0),
                toggle(off, block, 0),
                toggle(off, critical, 0),
                toggle(off, hit, 0)
            }}
        }}}},

        {<<"unbalancing_strike">>, {{0, {
            {seq(4), {
                {{add, {{single, -7}}, magic_cast_spec()}, {attr, dodge, def}, {stand, non_blowable}},
                {{add, {{single, -7}}, magic_cast_spec()}, {attr, block, def}, {stand, non_blowable}},
                {{add, {{single, -7}}, magic_cast_spec()}, {attr, hit, def}, {stand, non_blowable}}
            }}
        }}}}
    ],

    ets:insert(skills, Skills),
    ok.
