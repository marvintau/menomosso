drop table if exists player_supply_loot CASCADE;

create table player_supply_loot(
    loot_id         uuid        not null default uuid_generate_v4(),
    player_id       uuid        not null,
    supply_id       int         not null,
    acquire_time    timestamp   not null default now(),
    open_time       timestamp   not null default now(),
    is_opened       boolean     not null default 'no',
    buff1           boolean     not null default 'no',
    buff2           boolean     not null default 'no',
    buff3           boolean     not null default 'no',
    
    foreign key (player_id) references player(player_id),
    foreign key (supply_id) references supply_name(supply_id)
);

drop table if EXISTS loot_list cascade;
create table loot_list(
    loot_id uuid,
    item_id int,
    item_qty int
);

