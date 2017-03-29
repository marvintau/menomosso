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

physical_attack_spec() ->
    {physical, attack, non_absorbable, non_resistable, 0}.
dumb_attack_spec() ->
    {na, really, that, important}.

plain_attack() ->
    {{add, {{attr, attr, atk_range, off}}, physical_attack_spec()}, {attr, state, hp, def}}.

counter_attack(Times) ->
    {{add_inc_mul, {{attr, diff, state, off}, {single, Times}}, physical_attack_spec()}, {attr, state, hp, def}}.

buff(AttrType, Attr, Buff) ->
    {{add_mul, {{single, 1+Buff}}, none}, {attr, AttrType, Attr, off}}.

seq() ->
    seq(1).
seq(LastFor) ->
    seq(LastFor, casting).
seq(LastFor, Stage) ->
    seq(LastFor, Stage, []).
seq(LastFor, Stage, Conds) ->
    {{seq_norm, 0, LastFor, Stage}, Conds}.

next_damage() ->
    next_damage(1).
next_damage(LastFor) ->
    {{next_cast_norm, LastFor, {physical, attack, none, none}}, []}.

opponent_critical() ->
    {critical, '==', {attr, attr, outcome, def}}.


create_skills() ->
    true = ets:delete_all_objects(skills),

    Skills = [
        {single_attack, general, 2, [{0, [
            {seq(), [plain_attack()]}
        ]}]},
        {double_attack, general, 5, [{0, [
            {seq(), [plain_attack(), plain_attack()]}
        ]}]},
        {triple_attack, general, 9, [{0, [
            {seq(), [plain_attack(), plain_attack(), plain_attack()]}
        ]}]},

        {charm_of_foresight, general, 7, [{0, [
            {next_damage(), [buff(attr, dodge, 0.25), buff(attr, block, 0.25)]}
        ]}]},
        {fortify_armor, general, 3, [{0, [
            {next_damage(), [buff(attr, armor, 0.3)]}
        ]}]},
        {increase_crit, general, 3, [{0, [
            {next_damage(), [buff(attr, critical, 0.25)]}
        ]}]},

        {counter_back, general, 5, [{0, [
            {seq(2, casting, [opponent_critical()]), [counter_attack(3)]}
        ]}]},

        {healing_potion, general, 3, [{0, [
            {seq(), [{{add, {{range, 175, 255}}, dumb_attack_spec()}, {attr, state, hp, off}}]}
        ]}]}
    ],

    ets:insert(skills, Skills),
    ok.
