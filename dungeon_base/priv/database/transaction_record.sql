drop table if exists player_supply_loot CASCADE;

create table player_supply_loot(
    loot_id uuid,
    player_id uuid,
    supply_id int,
    acquire_time timestamp,
    open_time timestamp,
    is_opened boolean,
    buff1 boolean,
    buff2 boolean,
    buff3 boolean,
    
    foreign key (player_id) references players(player_id),
    foreign key (supply_id) references supply_name(supply_id)
);

drop table if EXISTS loot_list cascade;
create table loot_list(
    loot_id uuid,
    item_id int,
    item_qty int
);

