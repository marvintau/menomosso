drop table if exists player_battle_record cascade;
create table player_battle_record (
    battle_record_id uuid,
    self_id text,
    oppo_id text,
    self_card_id uuid,
    oppo_card_id uuid,
    self_preset_skill text[10],
    oppo_preset_skill text[10],

    result boolean,
    battle_record jsonb,
    last_created timestamp
)
