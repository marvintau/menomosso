CREATE EXTENSION if not exists "uuid-ossp";

-- 玩家ID和名称
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

    preset_card uuid,
    selected_skills text[10],

    rating double precision,
    ranking int,

    last_login TIMESTAMP,
    last_modified TIMESTAMP
);

drop type if exists Professions, RangeTypes, PrimTypes, SecdTypes cascade;
create type Professions as enum('warrior', 'rogue', 'hunter', 'mage');
create type RangeTypes as enum('near', 'far');
create type PrimTypes as enum('physical', 'magic');
create type SecdTypes as enum('physical', 'magic', 'bare', 'shield');

-- 卡牌对应职业的完整cast list不应当记入此数据结构中，为保持现有接口
-- 应当在服务器发送时按同样的JSON结构发给客户端。
drop table if exists cards cascade;
create table cards (
    card_id uuid, unique(card_id),
    card_name text,
    image_name text,
    level int,
    expi  int,
    stars int,

    profession Professions,
    range_type RangeTypes,

    hp int, armor int, agility int, hit int, block int, dodge int, resist int, critical int,

    atk_type PrimTypes, atk_max int, atk_min int,

    last_added TIMESTAMP,
    last_modified timestamp
);

-- 卡牌ID和持有此卡牌的玩家
-- 未来和用户相关的卡牌信息（譬如卡牌使用次数，卡牌等级，是否觉醒等等）
drop table if exists player_card_info cascade;
create table player_card_info (
    entry_id uuid, unique(entry_id),
    card_id uuid,
    player_id uuid,

    last_added TIMESTAMP,
    last_modified timestamp
);

insert into players
( player_id,                  player_name,        image_name,  association, expi,  player_level,  coins,   diamonds, preset_card,                            selected_skills,                                                                                                                                                             rating, ranking, last_login, last_modified) values
('8673cc53-e2a8-4375-b6a3-007e2ebe6d5f', 'Max Planc',               '1', '联盟',          1,             1,    100,   100,      '946ae77c-183b-4538-b439-ac9036024676', '{"single_attack", "double_attack", "triple_attack", "single_attack", "double_attack", "triple_attack", "single_attack", "double_attack", "none", "single_attack"}',          1000.5,     1,               now(),      now()),
('68b19bbe-bc2a-400f-b4e7-6e632b3b908f', 'Erwin Schodinger',        '1', '部落',          1,             1,    100,   100,      '1b0cf5e0-2164-46fd-8424-2146fca99fb9', '{"single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack", "single_attack"}', 1000.5,     1,               now(),      now());


insert into cards
(card_id,                                     card_name,  level,   expi,   stars, image_name,        profession, range_type, hp,   armor, agility, hit, block, dodge, resist, critical, atk_type, atk_max, atk_min, last_added, last_modified) values
('946ae77c-183b-4538-b439-ac9036024676', '普通刺客',     1,      1,       1, 'normal_rogue',    'rogue',    'near',     2700, 4500,  75,  35,  0,     30,    35,     30,     'physical',     350,     320,      now(),      now()),
('15d715a8-d585-48fc-a65a-286fc41c9a3f', '霸道刺客',     1,      1,       1, 'awaken_rogue',    'rogue',    'near',     2700, 4500,  75,  35,  0,     30,    35,     30,     'physical',     350,     320,      now(),      now()),
('a0c1a883-2995-4526-856c-26870e5b3f74', '普通猎人',     1,      1,       1, 'normal_hunter',   'hunter',   'far',      3400, 4500,  40,  35,  0,     30,    35,     30,     'physical',     350,     320,      now(),      now()),
('be2d65f0-3c93-457e-8180-de7c93a365a5', '痴呆猎人',     1,      1,       1, 'awaken_hunter',   'hunter',   'far',      3400, 4500,  40,  35,  0,     30,    35,     30,     'physical',     350,     320,      now(),      now()),
('a009e5e9-2057-4353-9871-309d68752c1b', '普通法师',     1,      1,       1, 'normal_mage',     'mage',     'far',      2300, 2700,  35,  20,  0,     20,    15,     35,     'physical',     350,     320,      now(),      now()),
('db1c75ca-aa32-4f2b-9bb1-355267d4a2ad', '暴躁法师',     1,      1,       1, 'awaken_mage',     'mage',     'far',      2300, 2700,  35,  20,  0,     20,    15,     35,     'physical',     350,     320,      now(),      now()),
('849d31be-b9cd-494c-9ccd-7cc656153b57', '普通战士',     1,      1,       1, 'normal_warrior',  'warrior',  'near',     3400, 4500,  50,  35,  30,    30,    35,     30,     'physical',     350,     320,      now(),      now()),
('1b0cf5e0-2164-46fd-8424-2146fca99fb9', '癫狂战士',     1,      1,       1, 'awaken_warrior',  'warrior',  'near',     3400, 4500,  50,  35,  30,    30,    35,     30,     'physical',     350,     320,      now(),      now());

select * from cards;

drop table if exists card_skills cascade;
create table card_skills (
    skill_name text,
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
('sure_hit',                       false,        4,      '849d31be-b9cd-494c-9ccd-7cc656153b57'),
('concussion',                     false,        3,      '849d31be-b9cd-494c-9ccd-7cc656153b57'),
('double_swing',                   false,        3,      '849d31be-b9cd-494c-9ccd-7cc656153b57'),
('first_aid',                      false,        1,      '849d31be-b9cd-494c-9ccd-7cc656153b57'),
('shield_wall',                    false,        4,      '849d31be-b9cd-494c-9ccd-7cc656153b57'),
('sure_hit',                       false,        4,      '1b0cf5e0-2164-46fd-8424-2146fca99fb9'),
('concussion',                     false,        3,      '1b0cf5e0-2164-46fd-8424-2146fca99fb9'),
('double_swing',                   false,        3,      '1b0cf5e0-2164-46fd-8424-2146fca99fb9'),
('first_aid',                      false,        1,      '1b0cf5e0-2164-46fd-8424-2146fca99fb9'),
('shield_wall',                    false,        4,      '1b0cf5e0-2164-46fd-8424-2146fca99fb9'),
('chain_lock',                     false,        6,      '946ae77c-183b-4538-b439-ac9036024676'),
('critical_strike',                false,        2,      '946ae77c-183b-4538-b439-ac9036024676'),
('deadly_strike',                  false,        7,      '946ae77c-183b-4538-b439-ac9036024676'),
('shield_breaker',                 false,        5,      '946ae77c-183b-4538-b439-ac9036024676'),
('unbalancing_strike',             false,        2,      '946ae77c-183b-4538-b439-ac9036024676'),
('chain_lock',                     false,        6,      '15d715a8-d585-48fc-a65a-286fc41c9a3f'),
('critical_strike',                false,        2,      '15d715a8-d585-48fc-a65a-286fc41c9a3f'),
('deadly_strike',                  false,        7,      '15d715a8-d585-48fc-a65a-286fc41c9a3f'),
('shield_breaker',                 false,        5,      '15d715a8-d585-48fc-a65a-286fc41c9a3f'),
('unbalancing_strike',             false,        2,      '15d715a8-d585-48fc-a65a-286fc41c9a3f');

select * from card_skills;


insert into player_card_info
select uuid_generate_v4(), cards.card_id cards_id, players.player_id players_id, now(), now() from cards cross join players;

drop table if exists chest_spec cascade;
create table chest_spec (
    chest_id int,
    chest_name CHARACTER VARYING(40),
    min_item_types int,
    max_item_types int,
    open_interval int,

    UNIQUE(chest_id)
);

insert into chest_spec(chest_id, chest_name, min_item_types, max_item_types, open_interval) values
(0000, '空气宝箱',    1, 1, 1),
(0001, '木质宝箱',    1, 1, 1),
(0002, '铁宝箱',      1, 2, 2),
(0003, '铜宝箱',      2, 2, 4),
(0004, '银宝箱',      2, 3, 8),
(0005, '金宝箱',      3, 3, 16);

drop table if exists supply_spec cascade;
create table supply_spec(
    supply_id int,
    supply_name character varying(40),
    cooldown_time int

    UNIQUE(supply_id)
)

insert into supply_spec(supply_id, supply_name, cooldown_time) values
(1,   '小型补给箱',   300),
(2,   '中型补给箱',   7200),
(3,   '大型补给箱',   21600);



drop table if exists supply_ cascade;

drop table if exists item_from_chest;
create table item_from_chest (
    item_id int,
    chest_id int,
    min_items int,
    max_items int,
    drop_rate int,

    foreign key(chest_id) references chest_spec(chest_id)
);

insert into item_from_chest(chest_id, item_id, min_items, max_items, drop_rate) values
(0001,2011,1,2,25), (0001,2021,1,2,25), (0001,2031,1,2,25), (0001,2041,1,2,25),
(0002,2011,1,2,15), (0002,2021,1,2,15), (0002,2031,1,2,15), (0002,2041,1,2,15),
(0002,2012,1,1,25), (0002,2022,1,1,25), (0002,2032,1,1,25), (0002,2042,1,1,25),
(0003,2012,1,1,25), (0003,2022,1,1,25), (0003,2032,1,1,25), (0003,2042,1,1,25),
(0003,2013,1,1,25), (0003,2023,1,1,25), (0003,2033,1,1,25), (0003,2043,1,1,25),
(0003,1001,1,1,15), (0003,1002,1,1,15), (0003,1003,1,1,15), (0003,1004,1,1,15),
(0004,1001,1,2,10), (0004,1002,1,2,10), (0004,1003,1,2,10), (0004,1004,1,2,10),
(0004,2013,1,3,20), (0004,2023,1,3,20), (0004,2033,1,3,20), (0004,2043,1,3,20),
(0004,2014,1,2,15), (0004,2024,1,2,15), (0004,2034,1,2,15), (0004,2044,1,2,15),
(0005,1001,1,4,20), (0005,1002,1,4,20), (0005,1003,1,4,20), (0005,1004,1,4,20),
(0005,2015,1,2,25), (0005,2025,1,2,25), (0005,2035,1,2,25), (0005,2045,1,2,25);

drop table if exists item_description;

create table item_description(
    item_id int,
    item_name character varying(40),
    item_desc character varying(255),
    image_name character varying(50)
);

insert into item_description values
(1001,'安杰丽卡的碎片','desc_placeholder','imagename_placeholder'),
(1002,'尼古拉斯的碎片','desc_placeholder','imagename_placeholder'),
(1003,'海菲尔德的碎片','desc_placeholder','imagename_placeholder'),
(1004,'艾米莉亚的碎片','desc_placeholder','imagename_placeholder'),
(2011,'战士的微小精华','desc_placeholder','imagename_placeholder'),
(2012,'战士的低级精华','desc_placeholder','imagename_placeholder'),
(2013,'战士的中级精华','desc_placeholder','imagename_placeholder'),
(2014,'战士的高级精华','desc_placeholder','imagename_placeholder'),
(2015,'战士的稀有精华','desc_placeholder','imagename_placeholder'),
(2021,'刺客的微小精华','desc_placeholder','imagename_placeholder'),
(2022,'刺客的低级精华','desc_placeholder','imagename_placeholder'),
(2023,'刺客的中级精华','desc_placeholder','imagename_placeholder'),
(2024,'刺客的高级精华','desc_placeholder','imagename_placeholder'),
(2025,'刺客的稀有精华','desc_placeholder','imagename_placeholder'),
(2031,'法师的微小精华','desc_placeholder','imagename_placeholder'),
(2032,'法师的低级精华','desc_placeholder','imagename_placeholder'),
(2033,'法师的中级精华','desc_placeholder','imagename_placeholder'),
(2034,'法师的高级精华','desc_placeholder','imagename_placeholder'),
(2035,'法师的稀有精华','desc_placeholder','imagename_placeholder'),
(2041,'猎人的微小精华','desc_placeholder','imagename_placeholder'),
(2042,'猎人的低级精华','desc_placeholder','imagename_placeholder'),
(2043,'猎人的中级精华','desc_placeholder','imagename_placeholder'),
(2044,'猎人的高级精华','desc_placeholder','imagename_placeholder'),
(2045,'猎人的稀有精华','desc_placeholder','imagename_placeholder');

drop table if exists char_chest cascade;
create table char_chest(
    char_id uuid,
    last_opened_chest int,
    last_opened_time TIMESTAMP,
    is_today_done bool,

    foreign key(char_id) references players(player_id),
    foreign key(last_opened_chest) references chest_spec(chest_id)
);

insert into char_chest(char_id, last_opened_chest, last_opened_time, is_today_done) values
('8673cc53-e2a8-4375-b6a3-007e2ebe6d5f', '0', now(), 'no'),
('68b19bbe-bc2a-400f-b4e7-6e632b3b908f', '0', now(), 'no');
