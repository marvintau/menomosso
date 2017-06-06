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
    get_player_rank/2,

    get_card/2,
    get_card_list/2,
    get_card_skills/2,
    get_card_battle/2,

    update_preset_card/2,
    update_selected_skills/2,
    update_rate/2,
    update_rank/2,
    update_frag/2,
    update_coin/2,
    update_level/2,
    update_card/2,
    update_card_level/2,

    check_chest_update/2,
    open_chest_update/2,

    add_supply/2,
    check_supply/2,
    open_supply/2
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 开始和结束数据库会话
connect() ->
    connect("localhost", "yuetao", "asdasdasd", "dungeon_base", 100).

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




%% ------------------------------------------------------------------------
%% 添加完整的玩家档案，添加玩家档案条目，添加玩家开宝箱档案条目，并返回
%% 玩家档案条目记录（来自players表）

add_new_player(Conn, _) ->
    dungeon_query_add_player:add_player(Conn).

add_player_card(Conn, {CardUUID, PlayerUUID}) ->
    dungeon_query_add_player:add_player_card(Conn, CardUUID, PlayerUUID).

%% ------------------------------------------------------------------------
%% 添加一个新卡牌，并且返回生成的ID

add_new_card(Conn, _) ->
    quickrand:seed(),
    CardID = uuid:uuid_to_string(uuid:get_v4_urandom()),

    Query = list_to_binary([
        "insert into cards(player_id, card_name, level, expi, image_name, profession,
         range_type, hp, armor, agility, hit, block, dodge, resist,
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
    % Query = list_to_binary(["select * from players, cards where players.preset_card=cards.card_id order by players.rating desc;"]),
    Query = list_to_binary(["select distinct players.*, cards.*, level, stars from players, cards, player_card_info where players.preset_card=player_card_info.card_id and players.preset_card=cards.card_id order by players.rating desc;"]),
    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, _, Players} -> 
            error_logger:info_report(Players),
            {ok, [dungeon_query_to_map:get_listed_player_map(Player) || Player <- Players]};
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
    QueryProfile = list_to_binary(["select * from players where player_id='", PlayerUUID,"';"]),

    Profile = epgsql:squery(Conn,binary_to_list(QueryProfile)),

    QueryCard = list_to_binary(["select cards.*, frags, level, stars, frags_required, coins_required from (
        select * from player_card_info, card_level_up
        where player_id = '", PlayerUUID, "' and player_card_info.level=card_level_up.card_level
        ) tem
        inner join cards on cards.card_id=tem.card_id;"]),

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

    QueryProfile = list_to_binary(["select * from players where player_id='", PlayerUUID,"';"]),
    {ok, _, [Player]} = epgsql:squery(Conn,binary_to_list(QueryProfile)),

    #{preset_card_id:=OffCardID} = PlayerMap = dungeon_query_to_map:get_player_map(Player),

    {ok, Card} = get_card_battle(Conn, {OffCardID}),

    {ok, maps:merge(PlayerMap, Card)}.


%% ------------------------------------------------------------------------
%% 得到玩家信息，包含玩家信息，和玩家所持的所有卡牌的具体信息
%% NOTE: 一般情况下主要用于自己

get_card_list(Conn, _) ->
    Query = list_to_binary(["select * from cards;"]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, _, Res} -> {ok, [dungeon_query_to_map:get_card_map(Card) || Card <- Res ]};
        _ -> {error, get_card_failed}
    end.

get_card(Conn, {CardUUID}) ->
    Query = list_to_binary(["select * from cards where card_id='",CardUUID , "';"]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, _, [Res]} -> {ok, dungeon_query_to_map:get_card_map(Res)};
        _ -> {error, get_card_failed}
    end.

get_card_battle(Conn, {CardUUID}) ->
    Query = list_to_binary(["select * from cards where card_id='",CardUUID , "';"]),

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
        where player_id = '", PlayerUUID, "';"]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, 1} -> {ok, preset_card_updated};
        _ -> {error, update_preset_card_failed}
    end.

%% ------------------------------------------------------------------------
%% 更新玩家的预设技能
update_selected_skills(Conn, {SkillList, SelfCardID, PlayerUUID}) ->

    ReformedSkillString = string:join(["\""++binary_to_list(SkillName)++"\"" || SkillName <- SkillList],","),

    Query = list_to_binary(["update players set preset_card= '", SelfCardID, "', selected_skills= '{", ReformedSkillString, "}', last_modified=now() where player_id = '", PlayerUUID, "';"]),

    case epgsql:squery(Conn,binary_to_list(Query)) of
        {ok, 1} -> {ok, selected_skills_updated};
        Error       ->
            erlang:display(Error),
            {error, update_selected_skills_failed}
    end.

%% ------------------------------------------------------------------------
%% 更新玩家的积分
%% NOTE: 服务器完成，不提供webAPI

update_rate(Conn, {Rate, PlayerUUID}) ->
    Query = list_to_binary(["update players set
        rating = ", integer_to_binary(Rate), ", last_modified=now() where player_id = '", PlayerUUID, "';"]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, 1} -> {ok, rate_updated};
        Error       ->
            error_logger:info_report(Error),
            {error, update_rate_failed}
    end.

update_coin(Conn, {CoinIncre, PlayerUUID}) ->
    Query = list_to_binary(["select coins from players where player_id='", PlayerUUID,"';"]),
    {ok, _, [{Coin}]} = epgsql:squery(Conn, binary_to_list(Query) ),

    QueryUpdate = list_to_binary(["update players set coins=", integer_to_binary(binary_to_integer(Coin) + CoinIncre), " where player_id='", PlayerUUID,"';"]),
    {ok, 1} = epgsql:squery(Conn, binary_to_list(QueryUpdate) ),

    QueryNew = list_to_binary(["select coins from players where player_id='", PlayerUUID,"';"]),
    {ok, _, [{CoinNew}]} = epgsql:squery(Conn, binary_to_list(QueryNew) ),
    erlang:display(CoinNew).

update_frag(Conn, {FragIncre, CardID, PlayerUUID}) ->
    Query = list_to_binary(["select player_id, card_id from player_card_info where player_id='", PlayerUUID, "';"]),
    {ok, _, Cards} = epgsql:squery(Conn, binary_to_list(Query)),

    case lists:keymember(CardID, 2, Cards) of
        false -> add_player_card(Conn, {CardID, PlayerUUID});
        _ ->
            QueryCheckFrags = list_to_binary(["select frags from player_card_info where player_id='", PlayerUUID, "' and card_id='", CardID, "';"]),
            {ok, _, [{Frags}]} = epgsql:squery(Conn, binary_to_list(QueryCheckFrags)),
            erlang:display({Frags, FragIncre}),
            FragsIntegerNew = binary_to_integer(Frags) + binary_to_integer(FragIncre),

            QueryAdd = list_to_binary(["update player_card_info set frags=", integer_to_binary(FragsIntegerNew), " where player_id='", PlayerUUID, "' and card_id='", CardID, "';"]),
            {ok, _} = epgsql:squery(Conn, binary_to_list(QueryAdd))
    end.

update_rank(Conn, {}) ->
    Query = "update players set ranking=row_number from (select player_id, rating, row_number() over (order by rating desc) from players) temp where players.player_id=temp.player_id;",

    case epgsql:squery(Conn, Query) of
        {ok, _} -> {ok, rank_updated};
        Error   ->
            error_logger:info_report(Error),
            {error, update_rank_failed}
    end.

get_player_rank(Conn, {PlayerUUID}) ->
    Query = list_to_binary(["select ranking from players where player_id='",PlayerUUID,"';"]),

    case epgsql:squery(Conn, binary_to_list(Query) ) of
        {ok, _, [Res]} -> {ok, Res};
        Error ->
            error_logger:info_report(Error),
            {error, get_player_rank_failed}
        end.

%% ------------------------------------------------------------------------
%% 更新玩家的级别
%% NOTE: 服务器完成，不提供webAPI

% update_level(Conn, {Level, PlayerUUID}) ->
%     Query = list_to_binary(["update players set
%         player_level = ", Level, ", last_modified=now()
%         where player_id = '", PlayerUUID, "';"]),

%     case epgsql:squery(Conn, binary_to_list(Query)) of
%         {ok, 1} -> {ok, level_updated};
%         _ -> {error, update_level_failed}
%     end.

update_level(Conn, {Level, PlayerUUID}) ->
    dungeon_query_player_level:update_level(Conn, Level, PlayerUUID).

%% ------------------------------------------------------------------------
%% 更新卡牌
update_card( Conn, {UpdatedProfile, CardUUID}) ->

    Query = list_to_binary(["
        update cards set
            card_name = '", maps:get(<<"card_name">>, UpdatedProfile), "',
            image_name = '", maps:get(<<"image_name">>, UpdatedProfile), "',
            profession = '", maps:get(<<"profession">>, UpdatedProfile), "',
            range_type = '", maps:get(<<"range_type">>, UpdatedProfile), "',
            hp = '", maps:get(<<"hp">>, UpdatedProfile), "',
            armor = '", maps:get(<<"armor">>, UpdatedProfile), "',
            agility = '", maps:get(<<"agility">>, UpdatedProfile), "',
            hit = '", maps:get(<<"hit">>, UpdatedProfile), "',
            block = '", maps:get(<<"block">>, UpdatedProfile), "',
            dodge = '", maps:get(<<"dodge">>, UpdatedProfile), "',
            resist = '", maps:get(<<"resist">>, UpdatedProfile), "',
            critical = '", maps:get(<<"critical">>, UpdatedProfile), "',
            atk_type = '", maps:get(<<"atk_type">>, UpdatedProfile), "',
            atk_max = '", maps:get(<<"atk_max">>, UpdatedProfile), "',
            atk_min = '", maps:get(<<"atk_min">>, UpdatedProfile), "',
            last_modified=now()
            where card_id='", CardUUID, "';"]),

    erlang:display(binary_to_list(Query)),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, 1} -> {ok, card_updated};
        Error ->
            erlang:display(Error),
            {error, update_card_failed}
    end.

update_card_level(Conn, {PlayerUUID, CardUUID}) ->
    QueryGetLevel = list_to_binary(["select level, frags, frags_required, coins_required from player_card_info where player_id='", PlayerUUID, "' and card_id='", CardUUID, "';"]),
    {ok, _, [{Level, CurrentFrags, FragsRequired, CoinsRequired}]} = epgsql:squery(Conn, binary_to_list(QueryGetLevel)),
    
    QueryGetCoin = list_to_binary(["select coins from players where player_id='", PlayerUUID,"';"]),
    {ok, _, [{CurrentCoins}]} = eqpsql:squery(Conn, binary_to_list(QueryGetCoin)),

    FragsInteger = binary_to_integer(CurrentFrags),
    FragsRequiredInteger = binary_to_integer(FragsRequired),
    CoinsInteger = binary_to_integer(CurrentCoins),
    CoinsRequiredInteger = binary_to_integer(CoinsRequired),


    case (FragsInteger >= FragsRequiredInteger) and (CoinsInteger >= CoinsRequiredInteger) of
        false ->
            {error, insufficient_frags_or_coins};
        _ ->
            QuerySetLevel = list_to_binary(["update player_card_info set level=", integer_to_binary(binary_to_integer(Level)+1),", 
                                            frags=", integer_to_binary(FragsInteger-FragsRequiredInteger),";"]),
            {ok, 1} = epqsql:squery(Conn, binary_to_list(QuerySetLevel)),

            QuerySetCoins = list_to_binary(["update players set coins=", integer_to_binary(CoinsInteger - CoinsRequiredInteger)," "]) ,
            {ok, 1} = epgsql:squery(Conn, binary_to_list(QuerySetCoins)),

            {ok, Level, CoinsInteger-CoinsRequiredInteger, FragsInteger-FragsRequiredInteger}
    end.




check_chest_update(Conn, {PlayerID}) ->
    dungeon_query_timed_chest:check_chest_and_update(Conn, PlayerID).

open_chest_update(Conn, {PlayerID}) ->
    dungeon_query_timed_chest:open_chest_and_update(Conn, PlayerID).


add_supply(Conn, {PlayerID, SupplyID}) ->
    dungeon_query_supply:add_new_supply(Conn, PlayerID, SupplyID).

check_supply(Conn, {PlayerID}) ->
    dungeon_query_supply:check_supply_remaining_time(Conn, PlayerID).

open_supply(Conn, {LootID}) ->
    dungeon_query_supply:open_supply(Conn, LootID).