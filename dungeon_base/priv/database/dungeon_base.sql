CREATE EXTENSION if not exists "uuid-ossp";

-------------------------------------------------------------
-- 玩家信息
-------------------------------------------------------------

drop table if exists players CASCADE;
create table players (
    player_id   uuid        not null default uuid_generate_v4(), unique(player_id),
    player_name varchar(20) not null default 'NEW',
    image_name  varchar(20) not null default '1',
    association varchar(20) not null default '联盟',
    
    expi            int not null default 1,
    player_level    int not null default 1,
    coins           int not null default 500,
    diamonds        int not null default 500,

    preset_card_id  uuid not null default '946ae77c-183b-4538-b439-ac9036024676',
    selected_skills text[10] not null default '{"single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack"}',

    rating  int not null default 9999,
    ranking int not null default 1000,

    quick_battle_counter int not null default 0,

    last_login TIMESTAMP not null default now(),
    last_modified TIMESTAMP not null default now()
);

insert into players
( player_id,                             player_name,         preset_card_id                        ) values
('8673cc53-e2a8-4375-b6a3-007e2ebe6d5f', 'Max Planc',         '946ae77c-183b-4538-b439-ac9036024676'),
('68b19bbe-bc2a-400f-b4e7-6e632b3b908f', 'Erwin Schodinger',  '1b0cf5e0-2164-46fd-8424-2146fca99fb9');



-------------------------------------------------------------
-- 卡牌信息
-------------------------------------------------------------

drop table if exists cards cascade;
create table cards (
    card_id     uuid not null default uuid_generate_v4(), unique(card_id),
    card_name   text not null default 'NEW',
    image_name  text not null default 'rogue',

    profession  text not null default 'rogue',
    range_type  text not null default 'near',

    hp          int not null default 3400,
    armor       int not null default 4500,
    agility     int not null default 50,
    hit         int not null default 35,
    block       int not null default 0,
    dodge       int not null default 35,
    resist      int not null default 35,
    critical    int not null default 30,

    atk_type    text not null default 'physical',
    atk_range   int4range not null default '[320, 350)',

    last_added      TIMESTAMP not null default now(),
    last_modified   timestamp not null default now()
);

insert into cards
(card_id,                              card_name,   image_name,  profession, range_type, hp,   armor, agility, hit, block, dodge, resist, critical, atk_type,   atk_range,   last_added, last_modified) values
('946ae77c-183b-4538-b439-ac9036024676', '普通刺客', 'rogue',    'rogue',    'near',      2700, 4500,  75,      35,  0,     30,    35,     30,       'physical', '[320, 350)',      now(),      now()),
('a0c1a883-2995-4526-856c-26870e5b3f74', '普通猎人', 'hunter',   'hunter',   'far',       3400, 4500,  40,      35,  0,     30,    35,     30,       'physical', '[320, 350)',      now(),      now()),
('a009e5e9-2057-4353-9871-309d68752c1b', '普通法师', 'mage',     'mage',     'far',       2300, 2700,  35,      20,  0,     20,    15,     35,       'physical', '[320, 350)',      now(),      now()),
('1b0cf5e0-2164-46fd-8424-2146fca99fb9', '癫狂战士', 'warrior',  'warrior',  'near',      3400, 4500,  50,      35,  30,    30,    35,     30,       'physical', '[320, 350)',      now(),      now());
  
select * from cards;

-------------------------------------------------------------
-- 与玩家相关的卡牌信息
-------------------------------------------------------------

drop table if exists player_card_info cascade;
create table player_card_info (
    player_card_id uuid primary key not null default uuid_generate_v4(), unique(player_card_id),
    
    card_id     uuid not null,
    player_id   uuid not null,
    
    frags        int not null default 1,
    card_level   int not null default 1,
    card_stars   int not null default 0,
    skill_points int not null default 0,

    last_added      TIMESTAMP not null default now(),
    last_modified   timestamp not null default now()
);

insert into player_card_info(card_id, player_id)
select cards.card_id cards_id, players.player_id player_id from cards cross join players;

select * from player_card_info;


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



drop table if exists player_battle_record cascade;
create table player_battle_record (
    battle_record_id uuid,
    self_id uuid,
    oppo_id uuid,
    self_card_id uuid,
    oppo_card_id uuid,
    self_preset_skill text[10],
    oppo_preset_skill text[10],

    result boolean,
    battle_record jsonb,
    last_created timestamp
)