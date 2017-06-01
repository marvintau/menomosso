defmodule Skills do

    def init_table() do
        :ets.new(:skills, [:set, :public, :named_table, {:read_concurrency, true}])
        create_skills()
    end

    def physical_attack_spec(), do: {:physical, :attack, :absorbable, :non_resistable, 0}
    def physical_attack_spec(absorbable), do: {:physical, :attack, absorbable, :non_resistable, 0}
    def physical_cast_spec(), do: {:physical, :cast, :absorbable, :non_resistable, 0}
 
    def magic_cast_spec(resis), do: {:magic, :cast, :non_absorbable, resis, 0}
    def magic_cast_spec(), do: magic_cast_spec(:non_resistable)


    def plain_attack(), do:         {{:add, {{:attr, :attr, :atk_range, :off}}, physical_attack_spec()}, {:attr, :state, :hp, :def}, {:chase, :blowable}}
    def plain_attack_no_blow(), do: {{:add, {{:attr, :attr, :atk_range, :off}}, physical_attack_spec()}, {:attr, :state, :hp, :def}, {:chase, :non_blowable}}
    def stand_plain_attack(), do:   {{:add, {{:attr, :attr, :atk_range, :off}}, physical_cast_spec()}, {:attr, :state, :hp, :def}, {:stand, :non_blowable}}

    def counter_attack(times), do: {{:add_inc_mul, {{:attr, :state, :diff, :off}, {:single, times}}, physical_attack_spec()}, {:attr, :state, :hp, :def}, {:stand, :blowable}}

    def buff(attr_type, attr, buff), do: {{:add_mul, {{:single, 1+buff}}, magic_cast_spec()}, {:attr, attr_type, attr, :off}, {:stand, :non_blowable}}
    def toggle(who, attr_type, attr, switch), do: {{:set, {{:single, switch}}, magic_cast_spec()}, {:attr, attr_type, attr, who}, {:stand, :non_blowable}}

    def seq(), do: seq(0)
    def seq(last_for), do: seq(last_for, :casting)
    def seq(last_for, stage), do: seq(last_for, stage, [])
    def seq(last_for, stage, conds), do: {{:seq_norm, 0, last_for, stage}, conds}

    def next_damage(), do: next_damage(0)
    def next_damage(last_for), do: next_damage(last_for, [])
    def next_damage(last_for, conds), do: next_damage(last_for, conds, :settling)
    def next_damage(last_for, conds, stage), do: {{:next_defense_norm, last_for, {:physical, :attack, :none, :none}, stage}, conds}

    def next_attack(), do: next_attack(1)
    def next_attack(last_for), do: {{:next_offense_norm, last_for, {:physical, :attack, :none, :none}, :settling}, []}

    def opponent_critical(), do: {:critical, '==', {:attr, :attr, :outcome, :def}}

    def create_skills() do
        true = :ets.delete_all_objects(:skills) 

        skills = [
            {:single_attack, [{0, [
                {seq(), [plain_attack()]}
            ]}]},

            {:double_attack, [{0, [
                {seq(), [stand_plain_attack(), plain_attack()]}
            ]}]},

            {:triple_attack, [{0, [
                {seq(), [stand_plain_attack(), stand_plain_attack(), plain_attack_no_blow()]}
            ]}]},

            {:charm_of_foresight, [{0, [
                {next_damage(), [buff(:attr, :dodge, 0.25), buff(:attr, :block, 0.25)]}
            ]}]},
            {:fortify_armor, [{0, [
                {next_damage(), [buff(:attr, :dodge, 0.3)]}
            ]}]},
            {:increase_crit, [{0, [
                {next_damage(), [buff(:attr, :critical, 0.25)]}
            ]}]},

            {:counterattack, [{0, [
                {next_damage(0, [opponent_critical()], :counter), [counter_attack(3)]}
            ]}]},

            {:healing_potion, [{0, [
                {seq(), [{{:add, {{:range, 175, 225}}, magic_cast_spec()}, {:attr, :state, :hp, :off}, {:back, :non_blowable}}]}
            ]}]},

            {:pierce_armor, [{0, [
                {seq(3),[{{:add_mul, {{:single, -0.5}}, magic_cast_spec()}, {:attr, :attr, :armor, :def}, {:stand, :non_blowable}}]}
            ]}]},

            {:poison_gas, [{0.5, [
                {seq(2), [
                    toggle(:def, :attr, :is_stunned, 1),
                    toggle(:def, :attr, :cast_disabled, 1),
                    toggle(:def, :attr, :block, 0),
                    toggle(:def, :attr, :dodge, 0),
                    toggle(:def, :attr, :resist, 0)
                ]}
            ]},{0.5,[
                {seq(2), [
                    toggle(:off, :attr, :is_stunned, 1),
                    toggle(:off, :attr, :cast_disabled, 1),
                    toggle(:off, :attr, :block, 0),
                    toggle(:off, :attr, :dodge, 0),
                    toggle(:off, :attr, :resist, 0)
                ]}
            ]}]},

            {:rune_of_the_void, [{0, [
                {seq(), [{{:set, {{:single, 1}}, magic_cast_spec()}, {:attr, :attr, :cast_disabled, :def}, {:stand, :non_blowable}}]}
            ]}]},

            {:holy_hand_grenade, [{0, [
                {seq(), [{{:add, {{:range, -500, -1}}, magic_cast_spec(:resistable)}, {:attr, :state, :hp, :def}, {:back, :non_blowable}}]}
            ]}]},

            {:talisman_of_shielding, [{0, [
                {next_damage(), [buff(:attr, :armor, 0.5)]}
            ]}]},

            {:talisman_of_death, [{0, [
                {seq(), [{{:add_mul, {{:single, -0.15}}, magic_cast_spec()}, {:attr, :state, :hp, :def}, {:back, :non_blowable}}]}
            ]}]},

            {:talisman_of_spellshrouding, [{0, [
                {next_damage(), [buff(:attr, :resist, 1)]}
            ]}]},

            {:sure_hit, [{0, [
                {next_attack(), [
                    toggle(:def, :attr, :resist, 0),
                    toggle(:def, :attr, :block, 0),
                    toggle(:def, :attr, :dodge, 0),
                    toggle(:def, :attr, :critical, 0),
                    toggle(:def, :attr, :hit, 0)
                ]}
            ]}]},

            {:concussion, [{0, [
                {seq(0), [
                    {{:add_inc_mul, {{:attr, :attr, :critical, :def}, {:single, -0.5}}, magic_cast_spec()}, {:attr, :state, :hp, :def}, {:stand, :non_blowable}},
                    {{:add_inc_mul, {{:attr, :attr, :agility, :def}, {:single, -1}}, physical_attack_spec(:absorbable)}, {:attr, :state, :hp, :def}, {:stand, :non_blowable}}
                ]}
            ]}]},

            {:double_swing, [{0, [
                {seq(2, :counter, []), [stand_plain_attack(), plain_attack()]}
            ]}]},

            {:first_aid, [{0, [
                {seq(), [
                    {{:add_inc_mul, {{:attr, :state, :hp, :off}, {:single, 0.08}}, magic_cast_spec()}, {:attr, :state, :hp, :off}, {:back, :non_blowable}}
                ]}
            ]}]},

            {:shield_wall, [{0, [
                {next_damage(), [
                    toggle(:off, :attr, :dodge, 0),
                    toggle(:off, :attr, :block, 120),
                    toggle(:def, :attr, :hit, 0),
                    toggle(:def, :attr, :critical, 0)
                ]}
            ]}]},

            {:chain_lock, [{0, [
                {seq(), [
                    toggle(:def, :attr, :cast_disabled, 1)
                ]}
            ]}]},

            {:critical_strike, [{0, [
                {next_attack(0), [
                    toggle(:def, :attr, :dodge, 0),
                    toggle(:def, :attr, :block, 0),
                    toggle(:off, :attr, :critical, 120),
                    toggle(:off, :attr, :hit, 0)
                ]}
            ]}]},

            {:deadly_strike, [{0, [
                {seq(), [
                    {{:add_inc_mul, {{:attr, :attr, :atk_range, :off}, {:single, 2.5}}, physical_attack_spec()}, {:attr, :state, :hp, :def}, {:chase, :blowable}}
                ]}
            ]}]},

            {:shield_breaker, [{0, [
                {seq(4), [
                toggle(:def, :attr, :block, 0)
                ]},
                {next_damage(), [
                    toggle(:off, :attr, :is_stunned, 1),
                    toggle(:off, :attr, :cast_disabled, 1),
                    toggle(:off, :attr, :dodge, 0),
                    toggle(:off, :attr, :block, 0),
                    toggle(:off, :attr, :critical, 0),
                    toggle(:off, :attr, :hit, 0)
                ]}
            ]}]},

            {:unbalancing_strike, [{0, [
                {seq(4), [
                    {{:add, {{:single, -7}}, magic_cast_spec()}, {:attr, :attr, :dodge, :def}, {:stand, :non_blowable}},
                    {{:add, {{:single, -7}}, magic_cast_spec()}, {:attr, :attr, :block, :def}, {:stand, :non_blowable}},
                    {{:add, {{:single, -7}}, magic_cast_spec()}, {:attr, :attr, :hit,   :def}, {:stand, :non_blowable}}
                ]}
            ]}]}
        ]

        :ets.insert(:skills, skills)
    end

end