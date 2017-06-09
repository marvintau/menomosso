CREATE EXTENSION if not exists "uuid-ossp";

-------------------------------------------------------------
-- 玩家信息
-------------------------------------------------------------

drop table if exists players CASCADE;
create table players (
    player_id uuid, unique(player_id),
    player_name varchar(20),
    image_name varchar(20),
    association varchar(20),
    expi int,
    player_level int,
    coins int,
    diamonds int,

    preset_card_id uuid,
    selected_skills text[10],

    rating  int,
    ranking int,

    quick_battle_counter int,

    last_login TIMESTAMP,
    last_modified TIMESTAMP
);

insert into players
( player_id,                             player_name,        image_name,  association, expi,  player_level,  coins,   diamonds, preset_card_id,                            selected_skills,                                                                                                                                                             rating, ranking, quick_battle_counter, last_login, last_modified) values
('8673cc53-e2a8-4375-b6a3-007e2ebe6d5f', 'Max Planc',               '1', '联盟',          1,             1,    100,   100,      '946ae77c-183b-4538-b439-ac9036024676', '{"single_attack", "double_attack", "triple_attack", "single_attack", "double_attack", "triple_attack", "single_attack", "double_attack", "none", "single_attack"}',          1000,     1,                        0, now(),      now()),
('68b19bbe-bc2a-400f-b4e7-6e632b3b908f', 'Erwin Schodinger',        '1', '部落',          1,             1,    100,   100,      '1b0cf5e0-2164-46fd-8424-2146fca99fb9', '{"single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack"}', 1000,     1,                        0, now(),      now());



-------------------------------------------------------------
-- 卡牌信息
-------------------------------------------------------------

drop type if exists Professions, RangeTypes, PrimTypes, SecdTypes cascade;
create type Professions as enum('warrior', 'rogue', 'hunter', 'mage');
create type RangeTypes as enum('near', 'far');
create type PrimTypes as enum('physical', 'magic');
create type SecdTypes as enum('physical', 'magic', 'bare', 'shield');

drop table if exists cards cascade;
create table cards (
    card_id uuid, unique(card_id),
    card_name text,
    image_name text,

    profession Professions,
    range_type RangeTypes,

    hp int, armor int, agility int, hit int, block int, dodge int, resist int, critical int,

    atk_type PrimTypes, atk_range int4range,

    last_added TIMESTAMP,
    last_modified timestamp
);

insert into cards
(card_id,                              card_name,   image_name,  profession, range_type, hp,   armor, agility, hit, block, dodge, resist,      critical, atk_type, atk_range, last_added, last_modified) values
('946ae77c-183b-4538-b439-ac9036024676', '普通刺客', 'rogue',    'rogue',    'near',      2700, 4500,  75,      35,  0,     30,    35,     30,     'physical',      '[320, 350)',      now(),      now()),
('a0c1a883-2995-4526-856c-26870e5b3f74', '普通猎人', 'hunter',   'hunter',   'far',       3400, 4500,  40,      35,  0,     30,    35,     30,     'physical',      '[320, 350)',      now(),      now()),
('a009e5e9-2057-4353-9871-309d68752c1b', '普通法师', 'mage',     'mage',     'far',       2300, 2700,  35,      20,  0,     20,    15,     35,     'physical',      '[320, 350)',      now(),      now()),
('1b0cf5e0-2164-46fd-8424-2146fca99fb9', '癫狂战士', 'warrior',  'warrior',  'near',      3400, 4500,  50,      35,  30,    30,    35,     30,     'physical',      '[320, 350)',      now(),      now());

select * from cards;

-------------------------------------------------------------
-- 与玩家相关的卡牌信息
-------------------------------------------------------------

drop table if exists player_card_info cascade;
create table player_card_info (
    player_card_id uuid primary key, unique(player_card_id),
    card_id uuid,
    player_id uuid,
    frags int default 1,
    level int,
    stars int,

    last_added TIMESTAMP,
    last_modified timestamp
);

insert into player_card_info
select uuid_generate_v4(), cards.card_id cards_id, players.player_id players_id, 1, 1, 0, now(), now() from cards cross join players;


-------------------------------------------------------------
-- 技能信息
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



-------------------------------------------------------------
-- 某位玩家某张卡的某个技能的信息
-------------------------------------------------------------

drop table if exists player_card_skill_info cascade;
create table player_card_skill_info (

    player_id uuid,
    card_id uuid,
    skill_name text,

    skill_level int
);

insert into player_card_skill_info
    select player_id, player_card_info.card_id, skill_name, 1 skill_level from card_skills cross join player_card_info;


drop table if exists card_level_up cascade;
create table card_level_up (card_level int, frags_required int, coins_required int);

insert into card_level_up(card_level, frags_required, coins_required) values
(1,        0,          0),
(2,        2,          5),
(3,        4,          20),
(4,        10,         50),
(5,        20,         150),
(6,        50,         400),
(7,        100,        1000),
(8,        200,        2000),
(9,        400,        4000),
(10,       800,        8000),
(11,       1000,       20000),
(12,       2000,       50000),
(13,       5000,       100000);