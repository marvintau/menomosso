-module(dungeon_query).

-author('Yue Marvin Tao').

-export([
    connect/0,
    connect/5,
    close/1,

    add_new_player/2,
    add_new_card/2,
    add_player_card/2,

    get_player/2,
    get_player_battle/2,
    get_player_list/2,
    get_player_card/2,

    get_card/2,
    get_card_battle/2,

    update_preset_card/2,
    update_selected_skills/2,
    update_ranking/2,
    update_level/2,
    update_card/2,

    check_chest_update/2,
    open_chest_update/2
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 开始和结束数据库会话
connect() ->
    connect("localhost", "yuetao", "asdasdasd", "dungeon", 100).

connect(Host, User, Password, Database, Timeout) ->
    erlang:display(connecting),

    case epgsql:connect(Host, User, Password, [{database, Database}, {timeout, Timeout}]) of
        {ok, Conn} ->
            erlang:display({'DungenBase', connected}),
            {ok, Conn};
        {error, Error} ->
            erlang:display({'DungenBase', connection, failed, Error}),
            {error, Error}
    end.

close(Conn) ->
    epgsql:close(Conn).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 向数据表内添加新的信息

%% ------------------------------------------------------------------------
%% 获得重复玩家名字
get_number_of_columns(Conn, PlayerName) ->
    Query = list_to_binary(["select count(player_name) from players where player_name like '", PlayerName, "%'"]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, _, [{ColumnNumber}]} -> {ok, ColumnNumber};
        _ -> {error, bad_column_number}
    end.


%% ------------------------------------------------------------------------
%% 随机生成名字
%% NOTE: 不要单独export

random_name() ->
    SurNames = [
        {1, <<"威猛的"/utf8>>}, {2, <<"霸道的"/utf8>>}, {3, <<"机智的"/utf8>>}, {4, <<"勇敢的"/utf8>>}, {5, <<"无敌的"/utf8>>},
        {6, <<"可爱的"/utf8>>}, {7, <<"呆萌的"/utf8>>}, {8, <<"天然的"/utf8>>}, {9, <<"干练的"/utf8>>}, {10, <<"飒爽的"/utf8>>}
    ],
    Given = [
        {1, <<"总裁"/utf8>>}, {2, <<"战士"/utf8>>}, {3, <<"魔法使"/utf8>>}, {4, <<"隐士"/utf8>>}, {5, <<"高人"/utf8>>},
        {6, <<"科学家"/utf8>>}, {7, <<"学霸"/utf8>>}, {8, <<"天才"/utf8>>}, {9, <<"学生会长"/utf8>>}, {10, <<"体育部长"/utf8>>}
    ],

    {_, [{_, ResSurname}|_]} = lists:splitwith(fun({ID, _Name}) -> ID < rand:uniform() * length(SurNames) end, SurNames),
    {_, [{_, ResGiven}|_]} = lists:splitwith(fun({ID, _Name}) -> ID < rand:uniform() * length(Given) end, Given),

    list_to_binary([ResSurname,ResGiven]).

%% ------------------------------------------------------------------------
%% 向players表内添加一个新的玩家条目
%% NOTE: 不要单独export

add_player(Conn, PlayerUUID, PlayerName) ->
    Query = list_to_binary(["insert into players values(
    '", PlayerUUID, "', '", PlayerName,"', '", integer_to_binary(round(rand:uniform())) ,"', 'league', 500, 1, 100, 100,
    '946ae77c-183b-4538-b439-ac9036024676',
    '{\"single_attack\", \"single_attack\", \"single_attack\", \"single_attack\", \"single_attack\",
      \"single_attack\", \"single_attack\", \"single_attack\", \"single_attack\", \"single_attack\"}',
    9999, now(), now()
    );"]),

    case epgsql:squery(Conn,binary_to_list(Query)) of
        {ok, 1} ->
            {ok, new_player_added};
        {error, Err} when element(3, Err) =:= <<"23514">> ->
            erlang:display(mismatch_preset_skill_length),
            {error, {add_player_failed, preset_skill, length}};
        Error ->
            erlang:display(Error),
            {error, add_player_failed}
    end.

%% ------------------------------------------------------------------------
%% 向char_chest表内添加一个新的玩家开宝箱记录
%% NOTE: 不单独export

add_chest_record(Conn, PlayerUUID) ->
    Query = list_to_binary([
        "insert into char_chest(char_id, last_opened_chest, last_opened_time, is_today_done) values ('", PlayerUUID, "', '0', now(), 'no')"
    ]),

    case epgsql:squery(Conn,binary_to_list(Query)) of
        {ok, 1} -> {ok, new_chest_record_created};
        _ -> {error, add_chest_record_failed}
    end.

%% ------------------------------------------------------------------------
%% 向player_card表内添加给定玩家ID的卡牌条目
%% NOTE: 不要单独export

add_player_card(Conn, {CardUUID, PlayerUUID}) ->
    Query = list_to_binary(["insert into player_card_info values
    (uuid_generate_v4(), '", CardUUID, "', '", PlayerUUID, "', now(), now());"]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, 1} -> {ok, new_card_added};
        _ -> {error, add_card_failed}
    end.


%% ------------------------------------------------------------------------
%% 添加完整的玩家档案，添加玩家档案条目，添加玩家开宝箱档案条目，并返回
%% 玩家档案条目记录（来自players表）

add_new_player(Conn, _) ->
    Name = random_name(),
    {ok, ColumnNumber} = get_number_of_columns(Conn, Name),
    erlang:display({duplicates, ColumnNumber}),

    CheckedName = case ColumnNumber of
        <<"0">> -> Name;
        Other -> list_to_binary([Name, Other])
    end,

    erlang:display({finalName, CheckedName}),

    quickrand:seed(),
    NewID = uuid:uuid_to_string(uuid:get_v4_urandom()),
    erlang:display({new_id_tobe_inserted, NewID}),

    {ok, new_player_added} = add_player(Conn, NewID, CheckedName),

    {ok, new_card_added} = add_player_card(Conn, {"946ae77c-183b-4538-b439-ac9036024676", NewID}),
    {ok, new_card_added} = add_player_card(Conn, {"15d715a8-d585-48fc-a65a-286fc41c9a3f", NewID}),
    {ok, new_card_added} = add_player_card(Conn, {"a0c1a883-2995-4526-856c-26870e5b3f74", NewID}),
    {ok, new_card_added} = add_player_card(Conn, {"be2d65f0-3c93-457e-8180-de7c93a365a5", NewID}),

    {ok, new_chest_record_created} = add_chest_record(Conn, NewID),

    {ok, NewID}.

%% ------------------------------------------------------------------------
%% 添加一个新卡牌，并且返回生成的ID

add_new_card(Conn, _) ->
    quickrand:seed(),
    CardID = uuid:uuid_to_string(uuid:get_v4_urandom()),

    Query = list_to_binary([
        "insert into cards(id, card_name, level, expi, image_name, profession,
         range_type, hp, armor, agi, hit, block, dodge, resist,
         critical, atk_type, atk_max, atk_min, last_added, last_modified) values
        ('", CardID, "', 'NEWCARD', 1, 100, 'normal_rogue','rogue',
        'near', 2700, 4500, 75, 35, 0, 30, 35, 30, 'physical',
        350, 300, now(), now())"]),

    {ok, 1} = epgsql:squery(Conn, binary_to_list(Query)),
    {ok, CardID}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ------------------------------------------------------------------------
%% 得到玩家列表，仅提供players的信息
%% TODO: 未来将会加上更多限定条件，譬如排名等，来限制获取的玩家数目

get_player_list(Conn, _) ->
    Query = list_to_binary(["select * from players, cards where players.preset_card=cards.id;"]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, _, Players} -> {ok, [dungeon_query_to_map:get_listed_player_map(Player) || Player <- Players]};
        Error ->
            error_logger:info_report(Error),
            {error, get_player_list_failed}
    end.


%% ------------------------------------------------------------------------
%% 得到玩家信息，包含玩家信息，和玩家所持的所有卡牌的具体信息
%% NOTE: 一般情况下主要用于自己

get_card_skills(Conn, {CardID}) ->
    Query = list_to_binary(["select skill_name, skill_multiple_time, skill_cost from card_skills where card_id in('", CardID,"', '00000000-0000-0000-0000-000000000000');"]),
    {ok, _, CardInfoRes} = epgsql:squery(Conn, binary_to_list(Query)),
    [#{skill_name=>SkillName, skill_multiple_time=>SKillMultipleTime, skill_cost=>binary_to_integer(SkillCost)} || {SkillName, SKillMultipleTime, SkillCost} <- CardInfoRes].

get_player(Conn, {PlayerUUID}) ->
    QueryProfile = list_to_binary(["select * from players where id='", PlayerUUID,"';"]),

    Profile = epgsql:squery(Conn,binary_to_list(QueryProfile)),

    QueryCard = list_to_binary(["select cards.* from (
        select * from player_card_info
        where player_id = '", PlayerUUID, "'
        ) tem
        inner join cards on cards.id=tem.card_id;"]),

    case Profile of
        {ok, _, [PlayerRes]} ->
            {ok, _, CardRes} = epgsql:squery(Conn,binary_to_list(QueryCard)),

            CardMapList = [dungeon_query_to_map:get_card_map(Card) || Card <- CardRes],
            CardMapWithSkills = [maps:put(skills, get_card_skills(Conn, {maps:get(id, CardMap)}), CardMap) || CardMap <- CardMapList],

            {ok, #{player_profile => dungeon_query_to_map:get_player_map(PlayerRes), card_profiles => CardMapWithSkills}};
        {ok, _, []} -> {error, player_not_found};
        _ -> {error, get_player_failed}
    end.

get_player_battle(Conn, {PlayerUUID}) ->

    QueryProfile = list_to_binary(["select * from players where id='", PlayerUUID,"';"]),
    {ok, _, [Player]} = epgsql:squery(Conn,binary_to_list(QueryProfile)),

    #{preset_card_id:=OffCardID} = PlayerMap = dungeon_query_to_map:get_player_map(Player),

    {ok, Card} = get_card_battle(Conn, {OffCardID}),

    {ok, maps:merge(PlayerMap, Card)}.


%% ------------------------------------------------------------------------
%% 得到玩家信息，包含玩家信息，和玩家所持的所有卡牌的具体信息
%% NOTE: 一般情况下主要用于自己

get_card(Conn, {CardUUID}) ->
    Query = list_to_binary(["select * from cards where id='",CardUUID , "';"]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, _, [Res]} -> {ok, dungeon_query_to_map:get_card_map(Res)};
        _ -> {error, get_card_failed}
    end.

get_card_battle(Conn, {CardUUID}) ->
    Query = list_to_binary(["select * from cards where id='",CardUUID , "';"]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, _, [Res]} -> {ok, dungeon_query_to_map:get_card_map_battle(Res)};
        _ -> {error, get_card_failed}
    end.

%% ------------------------------------------------------------------------
%% 获得某位玩家的所有卡牌ID
%% NOTE: consider to deprecate
get_player_card(Conn, {PlayerUUID}) ->
    Query = list_to_binary(["select card_id from player_card_info where player_id = '", PlayerUUID, "';"]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, _, [{ID}]} -> {ok, ID};
        _ -> {error, add_card_failed}
    end.


%% ------------------------------------------------------------------------
%% 更新玩家的预设卡牌ID

update_preset_card(Conn, {CardUUID, PlayerUUID}) ->
    Query = list_to_binary(["update players set
        preset_card = '", CardUUID, "', last_modified=now()
        where id = '", PlayerUUID, "';"]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, 1} -> {ok, preset_card_updated};
        _ -> {error, update_preset_card_failed}
    end.

%% ------------------------------------------------------------------------
%% 更新玩家的预设技能
update_selected_skills(Conn, {SkillList, PlayerUUID}) ->

    ReformedSkillString = string:join(["\""++binary_to_list(SkillName)++"\"" || SkillName <- SkillList],","),

    Query = list_to_binary(["update players set selected_skills= '{", ReformedSkillString, "}', last_modified=now() where id = '", PlayerUUID, "';"]),

    case epgsql:squery(Conn,binary_to_list(Query)) of
        {ok, 1} -> {ok, selected_skills_updated};
        Error       ->
            erlang:display(Error),
            {error, update_selected_skills_failed}
    end.

%% ------------------------------------------------------------------------
%% 更新玩家的排名
%% NOTE: 服务器完成，不提供webAPI

update_ranking(Conn, {Ranking, PlayerUUID}) ->
    Query = list_to_binary(["update players set
        player_ranking = ", Ranking, ", last_modified=now() where id = '", PlayerUUID, "';"]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, 1} -> {ok, ranking_updated};
        _       -> {error, update_ranking_failed}
    end.

%% ------------------------------------------------------------------------
%% 更新玩家的级别
%% NOTE: 服务器完成，不提供webAPI

update_level(Conn, {Level, PlayerUUID}) ->
    Query = list_to_binary(["update players set
        player_level = ", Level, ", last_modified=now()
        where id = '", PlayerUUID, "';"]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, 1} -> {ok, level_updated};
        _ -> {error, update_level_failed}
    end.

%% ------------------------------------------------------------------------
%% 更新卡牌
update_card( Conn, {UpdatedProfile, CardUUID}) ->

    Query = list_to_binary(["
        update cards set
        card_name='", maps:get(card_name, UpdatedProfile), "',
        image_name='", maps:get(image_name, UpdatedProfile), "',
        profession='", maps:get(profession, UpdatedProfile), "',
        range_type='", maps:get(range_type, UpdatedProfile), "',
        hp='", maps:get(hp, UpdatedProfile), "',
        armor='", maps:get(armor, UpdatedProfile), "',
        agi='", maps:get(agi, UpdatedProfile), "',
        hit='", maps:get(hit, UpdatedProfile), "',
        block='", maps:get(block, UpdatedProfile), "',
        dodge='", maps:get(dodge, UpdatedProfile), "',
        resist='", maps:get(resist, UpdatedProfile), "',
        critical='", maps:get(critical, UpdatedProfile), "',
        prim_type='", maps:get(prim_type, UpdatedProfile), "',
        prim_max='", maps:get(prim_max, UpdatedProfile), "',
        prim_min='", maps:get(prim_min, UpdatedProfile), "',
        secd_type='", maps:get(secd_type, UpdatedProfile), "',
        secd_max='", maps:get(secd_max, UpdatedProfile), "',
        secd_min='", maps:get(secd_min, UpdatedProfile), "',
            last_modified=now()
            where id='", CardUUID, "';"]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, 1} -> {ok, card_updated};
        _ -> {error, update_card_failed}
    end.


is_same_day(<<Mega:4/binary, Sec:6/binary, MilliSec/binary>>) ->

    LastTimeStamp = {binary_to_integer(Mega), binary_to_integer(Sec), binary_to_integer(MilliSec)},

    {LastDate, _} = calendar:now_to_datetime(os:timestamp()),
    {CurrDate, _} = calendar:now_to_datetime(LastTimeStamp),

    {LastDate == CurrDate, CurrDate}.

check_chest(Conn, PlayerUUID) ->
    Query = list_to_binary(["select
        char_id, last_opened_chest % 5 + 1, chest_name,
        date_part('epoch', (interval '1s' * open_interval - (now() - last_opened_time))) * interval '1s' as remaining,
        extract(epoch from last_opened_time) * 100000 as last_opened_time, is_today_done
    from
        char_chest
        inner join chest_spec on char_chest.last_opened_chest % 5 + 1 = chest_spec.chest_id
        where
    char_id = '", PlayerUUID, "';"]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, _, [{ID, NextChestID, NextName, Remaining, LastOpen, IsTodayDone}]} ->
            {ok, {ID, NextChestID, NextName, Remaining, IsTodayDone}, is_same_day(LastOpen)};
        Error ->
        error_logger:info_report(Error),
        {error, check_chest_failed}
    end.

next_day_reset(Conn, PlayerUUID) ->
    Query = list_to_binary(["update char_chest
    set
        last_opened_chest = 0,
        last_opened_time = now() + age(now()),
        is_today_done = 'no'
    where char_id = '", PlayerUUID,"';"]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, 1} -> {ok, next_day_reset};
        _ -> {error, next_day_reset_failed}
    end.

is_okay_to_open(Conn, PlayerUUID) ->
    Query = list_to_binary(["select
        ((interval '0' >= interval '1s' * open_interval - (now() - last_opened_time)) AND (NOT is_today_done)) as remaining
    from
        char_chest
        inner join chest_spec on char_chest.last_opened_chest % 5 + 1 = chest_spec.chest_id
        where
    char_id = '", PlayerUUID, "';"]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, _, [{<<"t">>}]} -> {ok, okay_to_open};
        {ok, _, [{<<"f">>}]} -> {ok, not_okay_to_open};
        _ -> {error, is_okay_to_open_failed}
    end.



open_chest(Conn, PlayerUUID) ->
    Query = list_to_binary(["update char_chest
        set
            last_opened_chest = last_opened_chest % 5 + 1,
            last_opened_time = now(),
            is_today_done = CASE WHEN (last_opened_chest = 4) or is_today_done THEN true ELSE false END
        where char_id = '", PlayerUUID, "';"
    ]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, 1} -> {ok, opened_chest};
        _ -> {error, open_chest_failed}
    end.

get_chest_item_types(Conn, PlayerUUID) ->
    Query = list_to_binary(["
        select chest_id, round(random() * (max_item_types - min_item_types)) + min_item_types as item_types
    from
        char_chest
    inner join chest_spec on char_chest.last_opened_chest = chest_spec.chest_id
    where char_id = '", PlayerUUID, "';"]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, _, [{ChestID, DroppedNumberBin}]} -> {ok, {ChestID, DroppedNumberBin}};
        _ -> {error, add_card_failed}
    end.

get_chest_items(Conn, ChestID) ->
    Query = list_to_binary(["select item_id, item_name, items
    FROM
    (SELECT
        tem.item_id, item_name, items, generate_series(1, drop_rate/5) as nah
    from
        (select
            chest_id, item_id, drop_rate,
            round(random() * (max_items - min_items) + min_items) as items
        from
            item_from_chest
        where chest_id=", ChestID, "
        ) as tem
    inner join
        item_description on tem.item_id = item_description.item_id) as populated
    group by item_id, item_name, items
    order by random();"]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, _, Items} -> {ok, Items};
        _ -> {error, add_card_failed}
    end.


check_chest_to_map({ID, ChestID, NextChestName, NextOpenTime, IsSameDay}) ->
    {[{id, ID}, {next_chest_id, ChestID}, {next_chest_name, NextChestName}, {next_open_time, NextOpenTime}, {is_same_day, IsSameDay}]}.


check_chest_update(Conn, {PlayerID}) ->
    {ok, CheckResult, IsSameDay} = check_chest(Conn, PlayerID),

    case IsSameDay of
        <<"f">> ->
            {ok, next_day_reset} = next_day_reset(Conn, PlayerID),
            {ok, NewCheckResult, _IsSameDay} = check_chest(Conn, PlayerID),
            {ok, NewCheckResult};
        _ ->
            {ok, check_chest_to_map(CheckResult)}
    end.

open_chest_update(Conn, {PlayerID}) ->
    {_, IsOkayToOpen} = is_okay_to_open(Conn, PlayerID),

    {_Status, Res} = case IsOkayToOpen of
        okay_to_open ->
            {ok, opened_chest} = open_chest(Conn, PlayerID),
            {ok, {ChestID, _DroppedNumberBin}} = get_chest_item_types(Conn, PlayerID),
            get_chest_items(Conn, ChestID);
        not_okay_to_open ->
            {ok, not_okay_to_open};
        Reason ->
            {error, Reason}
        end,
    {ok, Res}.
