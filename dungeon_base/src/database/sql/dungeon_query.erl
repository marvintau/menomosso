-module(dungeon_query).

-author('Yue Marvin Tao').

-export([
    connect/0,
    connect/5,
    close/1,

    add_new_player/2,
    add_player_card/2,

    get_player/2,
    get_player_battle/2,
    get_player_list/2,
    get_player_rank/2,

    update_selected_skills/2,
    update_rate/2,
    update_rank/2,
    update_frag/2,
    update_coin/2,
    update_level/2,
    update_card/2,
    update_card_level/2,

    update_quick_battle_counter/2,

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ------------------------------------------------------------------------
%% 得到玩家列表，仅提供players的信息
%% TODO: 未来将会加上更多限定条件，譬如排名等，来限制获取的玩家数目

get_player_list(Conn, _) ->
    Query = list_to_binary(["
        select distinct players.*, cards.*, card_level, card_stars from players, cards, player_card_info
        where players.preset_card_id=player_card_info.card_id and players.preset_card_id=cards.card_id order by players.rating desc;
    "]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, ColumnSpec, Players} -> 
            error_logger:info_report(util:get_mapped_records(ColumnSpec, Players)),
            {ok, util:get_mapped_records(ColumnSpec, Players)};
        Error ->
            error_logger:info_report(Error),
            {error, get_player_list_failed}
    end.


%% ------------------------------------------------------------------------
%% 得到玩家信息，包含玩家信息，和玩家所持的所有卡牌的具体信息
%% NOTE: 一般情况下主要用于自己

get_player(Conn, {PlayerUUID}) ->
    dungeon_query_get_player_info:get_player_info(Conn, PlayerUUID).


get_player_battle(Conn, {PlayerUUID}) ->
    dungeon_query_get_battle_context:get_battle_context(Conn, PlayerUUID).


%% ------------------------------------------------------------------------
%% 更新玩家的预设技能
update_selected_skills(Conn, {SkillList, SelfCardID, PlayerUUID}) ->

    ReformedSkillString = string:join(["\""++binary_to_list(SkillName)++"\"" || SkillName <- SkillList],","),

    Query = list_to_binary(["update players set preset_card_id= '", SelfCardID, "', selected_skills= '{", ReformedSkillString, "}', last_modified=now() where player_id = '", PlayerUUID, "';"]),

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
    QueryUpdate = list_to_binary(["update players set coins=coins+", integer_to_binary(CoinIncre), " where player_id='", PlayerUUID,"';"]),
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
            QueryAdd = list_to_binary(["update player_card_info set frags=frags+", binary_to_integer(FragIncre), " where player_id='", PlayerUUID, "' and card_id='", CardID, "';"]),
            {ok, _} = epgsql:squery(Conn, binary_to_list(QueryAdd))
    end.

update_rank(Conn, {}) ->
    Query = "update players set ranking=row_number from 
                (select player_id, rating, row_number() over (order by rating desc) from players)
                temp where players.player_id=temp.player_id;",

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
    QueryGetLevel = list_to_binary(["select card_level, frags, frags_required, coins_required from player_card_info where player_id='", PlayerUUID, "' and card_id='", CardUUID, "';"]),
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
            QuerySetLevel = list_to_binary(["update player_card_info set level=level+1, 
                                            frags=", integer_to_binary(FragsInteger-FragsRequiredInteger),";"]),
            {ok, 1} = epqsql:squery(Conn, binary_to_list(QuerySetLevel)),

            QuerySetCoins = list_to_binary(["update players set coins=", integer_to_binary(CoinsInteger - CoinsRequiredInteger)," "]) ,
            {ok, 1} = epgsql:squery(Conn, binary_to_list(QuerySetCoins)),

            {ok, Level, CoinsInteger-CoinsRequiredInteger, FragsInteger-FragsRequiredInteger}
    end.


update_quick_battle_counter(Conn, {PlayerUUID}) ->
    QueryUpdate = list_to_binary(["update players set quick_battle_counter=(quick_battle_counter+1)%5 where player_id='", PlayerUUID,"';"]) ,
    {ok, 1} = epgsql:squery(Conn, binary_to_list(QueryUpdate)),

    Query = list_to_binary(["select quick_battle_counter from players where player_id='", PlayerUUID, "';"]),
    {ok, _, [{Res}]} = epgsql:squery(Conn, binary_to_list(Query)),
    binary_to_integer(Res).

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