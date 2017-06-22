-------------------------------------------------------------
-- 与玩家相关的卡牌信息
-------------------------------------------------------------

drop table if exists player_obtained_card cascade;
create table player_obtained_card (
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

insert into player_obtained_card(card_id, player_id)
select cards.card_id cards_id, player.player_id player_id from cards cross join player;

select * from player_obtained_card;


