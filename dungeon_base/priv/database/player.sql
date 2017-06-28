-------------------------------------------------------------
-- 玩家信息 
-------------------------------------------------------------

drop table if exists player CASCADE;
create table player (
    player_id   text        not null default 'default',
    player_name varchar(20) not null default 'NEW',
    image_name  varchar(20) not null default '1',
    association varchar(20) not null default '联盟',
    
    expi            int not null default 1,
    player_level    int not null default 1,
    coins           int not null default 500,
    diamonds        int not null default 500,

    preset_card_id  uuid not null default '946ae77c-183b-4538-b439-ac9036024676',

    rating  int not null default 9999,
    ranking int not null default 1000,

    quick_battle_counter int not null default 0,

    last_login TIMESTAMP not null default now(),
    last_modified TIMESTAMP not null default now()
);

insert into player
( player_id,                             player_name,         preset_card_id                        ) values
('8673cc53-e2a8-4375-b6a3-007e2ebe6d5f', 'Max Planc',         '946ae77c-183b-4538-b439-ac9036024676'),
('68b19bbe-bc2a-400f-b4e7-6e632b3b908f', 'Erwin Schodinger',  '1b0cf5e0-2164-46fd-8424-2146fca99fb9');

select * from player;
