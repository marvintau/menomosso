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
