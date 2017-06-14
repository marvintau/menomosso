drop table if exists supply_name cascade;
create table supply_name(
    supply_id int,
    supply_name character varying(40),
    cooldown_time int,

    UNIQUE(supply_id)
);

insert into supply_name(supply_id, supply_name, cooldown_time) values
(1,   '小型补给箱',   3),
(2,   '中型补给箱',   3),
(3,   '大型补给箱',   3);



drop table if exists supply_spec cascade;
create table supply_spec(
    supply_id int,
    item_id int,
    max_qty int,
    min_qty int,
    drop_rate int,
    item_group int,

    foreign key(supply_id) references supply_name(supply_id)
);

insert into supply_spec(supply_id, item_id, max_qty, min_qty, drop_rate, item_group) values
(1,   1,   5000,4000,1,  1),
(1,   2,   300, 200, 1,  2),
(1,   3,   300, 200, 1,  2),
(1,   4,   300, 200, 1,  2),
(1,   5,   300, 200, 1,  2),
(2,   1,   5000,4000,1,  1),
(2,   2,   300, 200, 1,  2),
(2,   3,   300, 200, 1,  2),
(2,   4,   300, 200, 1,  2),
(2,   5,   300, 200, 1,  2),
(3,   1,   5000,4000,1,  1),
(3,   2,   300, 200, 1,  2),
(3,   3,   300, 200, 1,  2),
(3,   4,   300, 200, 1,  2),
(3,   5,   300, 200, 1,  2);

drop table if exists supply_item_types cascade;

create table supply_item_types(
    item_type_id int, unique(item_type_id),
    item_type_name character varying(20),
    is_duplicable boolean,
    is_stackable boolean
);

insert into supply_item_types(item_type_id,    item_type_name,    is_duplicable,    is_stackable) values
(1,   'Gold',           false,   true),
(2,   'Fragment',       false,   true);

drop table if exists supply_items cascade;
create table supply_items(
    item_id int,
    item_name character varying(20),
    description character varying(20),
    item_type_id int,

    foreign key(item_type_id) references supply_item_types(item_type_id)
);

insert into supply_items(item_id, item_name, description, item_type_id) values
(1,   '金币',        '',    1),
(2,   '刺客的碎片',   '',    2),
(3,   '战士的碎片',   '',    2),
(4,   '法师的碎片',   '',    2),
(5,   '猎人的碎片',   '',    2);
