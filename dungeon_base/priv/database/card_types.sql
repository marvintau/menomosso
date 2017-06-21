-------------------------------------------------------------
-- 卡牌类型信息
-------------------------------------------------------------

drop table if exists card_types cascade;
create table card_types (
    card_id     uuid not null default uuid_generate_v4(), unique(card_id),
    card_name   text not null default 'NEW',
    image_name  text not null default 'rogue',

    profession  text not null default 'rogue',
    range_type  text not null default 'near',

    atk_type    text not null default 'physical',

    last_added      TIMESTAMP not null default now(),
    last_modified   timestamp not null default now()
);

insert into card_types
(card_id,                                card_name,   image_name,  profession, range_type,  atk_type,   last_added, last_modified) values
('946ae77c-183b-4538-b439-ac9036024676', '普通刺客', 'rogue',    'rogue',    'near',      'physical',       now(),      now()),
('a0c1a883-2995-4526-856c-26870e5b3f74', '普通猎人', 'hunter',   'hunter',   'far',       'physical',       now(),      now()),
('a009e5e9-2057-4353-9871-309d68752c1b', '普通法师', 'mage',     'mage',     'far',       'physical',       now(),      now()),
('1b0cf5e0-2164-46fd-8424-2146fca99fb9', '癫狂战士', 'warrior',  'warrior',  'near',      'physical',       now(),      now());
  
select * from card_types;


