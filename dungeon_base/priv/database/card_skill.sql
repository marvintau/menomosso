-------------------------------------------------------------
-- 单张卡牌的技能信息
-------------------------------------------------------------

drop table if exists card_skills cascade;
create table card_skills (

    skill_name text, unique(skill_name),
    skill_multiple_time boolean,
    skill_cost int,
    card_id uuid
);

insert into card_skills(skill_name, skill_multiple_time, skill_cost, card_id) values
('single_attack',                  true,         2,      '00000000-0000-0000-0000-000000000000'),
('double_attack',                  true,         5,      '00000000-0000-0000-0000-000000000000'),
('triple_attack',                  true,         9,      '00000000-0000-0000-0000-000000000000'),
('charm_of_foresight',             false,        7,      '00000000-0000-0000-0000-000000000000'),
('fortify_armor',                  false,        3,      '00000000-0000-0000-0000-000000000000'),
('increase_crit',                  false,        3,      '00000000-0000-0000-0000-000000000000'),
('counterattack',                  false,        5,      '00000000-0000-0000-0000-000000000000'),
('healing_potion',                 false,        3,      '00000000-0000-0000-0000-000000000000'),
('holy_hand_grenade',              false,        2,      '00000000-0000-0000-0000-000000000000'),
('pierce_armor',                   false,        2,      '00000000-0000-0000-0000-000000000000'),
('poison_gas',                     false,        1,      '00000000-0000-0000-0000-000000000000'),
('rune_of_the_void',               false,        9,      '00000000-0000-0000-0000-000000000000'),
('talisman_of_death',              false,        7,      '00000000-0000-0000-0000-000000000000'),
('talisman_of_shielding',          false,        3,      '00000000-0000-0000-0000-000000000000'),
('talisman_of_spellshrouding',     false,        7,      '00000000-0000-0000-0000-000000000000'),
('sure_hit',                       false,        4,      '1b0cf5e0-2164-46fd-8424-2146fca99fb9'),
('concussion',                     false,        3,      '1b0cf5e0-2164-46fd-8424-2146fca99fb9'),
('double_swing',                   false,        3,      '1b0cf5e0-2164-46fd-8424-2146fca99fb9'),
('first_aid',                      false,        1,      '1b0cf5e0-2164-46fd-8424-2146fca99fb9'),
('shield_wall',                    false,        4,      '1b0cf5e0-2164-46fd-8424-2146fca99fb9'),
('chain_lock',                     false,        6,      '946ae77c-183b-4538-b439-ac9036024676'),
('critical_strike',                false,        2,      '946ae77c-183b-4538-b439-ac9036024676'),
('deadly_strike',                  false,        7,      '946ae77c-183b-4538-b439-ac9036024676'),
('shield_breaker',                 false,        5,      '946ae77c-183b-4538-b439-ac9036024676'),
('unbalancing_strike',             false,        2,      '946ae77c-183b-4538-b439-ac9036024676');

select * from card_skills;


