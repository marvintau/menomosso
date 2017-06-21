-------------------------------------------------------------
-- 某位玩家某张卡的某个技能的信息
-------------------------------------------------------------

drop table if exists player_obtained_card_skills cascade;
create table player_obtained_card_skills (

    player_card_id uuid,
    player_id uuid,
    card_id uuid,
    skill_name text,
    skill_multiple_time boolean,
    skill_cost int,
    skill_level int default 1
);

insert into player_obtained_card_skills
    select player_card_id, player_id, player_card_info.card_id, skill_name, skill_multiple_time, skill_cost, 1 as skill_level from card_skills cross join player_card_info;


