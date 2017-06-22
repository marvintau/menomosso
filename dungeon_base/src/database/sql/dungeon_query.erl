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
    update_card_skill_level/2,

    update_quick_battle_counter/2,

    check_chest_update/2,
    open_chest_update/2,

    add_supply/2,
    check_supply/2,
    open_supply/2,

    store_battle_record/2,
    fetch_battle_record_list/2,
    fetch_battle_record/2
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

add_player_obtained_card(Conn, PlayerID, CardID) ->
    {ok, [#{player_card_id:=PlayerCardID}]} = player_obtained_cards:add(Conn, PlayerID, CardID),

    {ok, CardSkills} = card_skills:get(Conn),
    SelectedCardSkills = [
        #{skill_name          =>SkillName,
          skill_multiple_time =>SkillMultipleTime,
          skill_cost          =>SkillCost}
        || #{skill_name:=SkillName,
             skill_multiple_time:=SkillMultipleTime,
             skill_cost:=SkillCost,
             card_id:=ID} <- CardSkills, (ID =:= <<"00000000-0000-0000-0000-000000000000">>) or (ID =:= CardID)],

    SkillsTobeInserted = [
        Skills#{player_card_id=>PlayerCardID,
                player_id=>PlayerID,
                card_id=>CardID}
        || Skills <- SelectedCardSkills],
    
    player_obtained_card_skills:add(Conn, SkillsTobeInserted).


add_new_player(Conn, _) ->
    quickrand:seed(),
    NewID = list_to_binary(uuid:uuid_to_string(uuid:get_v4_urandom())),
    erlang:display({new_id_tobe_inserted, NewID}),

    {ok, added} = player:add(Conn, NewID),
    {ok, Res} = player:get(Conn, NewID),
    error_logger:info_report(Res),

    add_player_obtained_card(Conn, NewID, <<"946ae77c-183b-4538-b439-ac9036024676">>),
    add_player_obtained_card(Conn, NewID, <<"1b0cf5e0-2164-46fd-8424-2146fca99fb9">>),
 
    {ok, NewID}.


add_player_card(Conn, {CardUUID, PlayerUUID}) ->
    dungeon_query_add_player:add_player_card(Conn, CardUUID, PlayerUUID).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ------------------------------------------------------------------------
%% 得到玩家列表，仅提供players的信息
%% TODO: 未来将会加上更多限定条件，譬如排名等，来限制获取的玩家数目

get_single_listed_player(Conn, PlayerID) ->

    {ok, #{preset_card_id:=PresetCardID}=PlayerResult} = player:get(Conn, PlayerID),
    
    PlayerCardInfo = card_detail:get(Conn, PlayerID, PresetCardID),

    maps:remove(skills, maps:merge(PlayerResult, PlayerCardInfo)).

get_player_list(Conn, _) ->

    {ok, PlayersResult} = player:get(Conn),
    SortedPlayerResult  = lists:sort(fun(#{rating:=RatingA}, #{rating:=RatingB}) -> RatingA < RatingB end, PlayersResult),
    SortedPlayerID      = lists:map(fun(#{player_id:=PlayerID}) -> PlayerID end, SortedPlayerResult),
    SortedPlayers       = lists:map(fun(PlayerID) -> get_single_listed_player(Conn, PlayerID) end, SortedPlayerID),

    error_logger:info_report(SortedPlayers),
    {ok, SortedPlayers}.

%% ------------------------------------------------------------------------
%% 得到玩家信息，包含玩家信息，和玩家所持的所有卡牌的具体信息
%% NOTE: 一般情况下主要用于自己

get_player(Conn, {PlayerUUID}) ->
    dungeon_query_get_player_info:get_player_info(Conn, PlayerUUID).


get_player_battle(Conn, {PlayerUUID}) ->
    battle_context:get(Conn, PlayerUUID);
get_player_battle(Conn, {PlayerUUID, CardID, SelectedSkills}) ->
    battle_context:get(Conn, PlayerUUID, CardID, SelectedSkills).


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

    Query = util:set_query(<<"players">>,
        #{rating=>Rate,
          last_modified=><<"now()">>
         },
        #{player_id=>PlayerUUID}
    ),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, 1} -> {ok, rate_updated};
        Error       ->
            error_logger:info_report(Error),
            {error, update_rate_failed}
    end.

update_coin(Conn, {CoinIncre, PlayerUUID}) ->
    
    Query = list_to_binary([
        util:set(<<"players">>, #{coins=> list_to_binary([<<"coins+">>, integer_to_binary(CoinIncre)])}, #{player_id=>PlayerUUID})
    ]),
    
    epgsql:squery(Conn, Query).

update_frag(Conn, {FragIncre, CardID, PlayerUUID}) ->
    Query = list_to_binary(["select player_id, card_id from player_obtained_card where player_id='", PlayerUUID, "';"]),
    {ok, _, Cards} = epgsql:squery(Conn, binary_to_list(Query)),

    case lists:keymember(CardID, 2, Cards) of
        false -> add_player_card(Conn, {CardID, PlayerUUID});
        _ ->
            erlang:display(FragIncre),
            QueryAdd = list_to_binary(["update player_obtained_card set frags=frags+", FragIncre, " where player_id='", PlayerUUID, "' and card_id='", CardID, "';"]),
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

get_card_info_for_update_level(Conn, {PlayerUUID, CardUUID}) ->
    QueryGetLevel = list_to_binary(["select card_level, frags from player_obtained_card where player_id='", PlayerUUID, "' and card_id='", CardUUID, "';"]),
    error_logger:info_report({PlayerUUID, CardUUID}),
    {ok, _, [{CurrLevel, CurrentFrags}]} = epgsql:squery(Conn, binary_to_list(QueryGetLevel)),
    
    QueryGetCoin = list_to_binary(["select coins from players where player_id='", PlayerUUID,"';"]),
    {ok, _, [{CurrentCoins}]} = epgsql:squery(Conn, binary_to_list(QueryGetCoin)),

    QueryGetRequired = list_to_binary(["select * from card_level_spec where level=", CurrLevel," and card_id='", CardUUID,"';" ]),
    {ok, Columns, Result} = epgsql:squery(Conn, binary_to_list(QueryGetRequired)),

    [NewLevelSpecs] = util:get_mapped_records(Columns, Result),

    FragsInteger = binary_to_integer(CurrentFrags),
    CoinsInteger = binary_to_integer(CurrentCoins),

    {CurrLevel, FragsInteger, CoinsInteger, NewLevelSpecs}.


actual_update_card_level(Conn, {Frags, FragsRequired, Coins, CoinsRequired, PlayerUUID, CardUUID}) ->
    QuerySetLevel = list_to_binary(["update player_obtained_card set card_level=card_level+1, 
                                    frags=", integer_to_binary(Frags-FragsRequired)," where player_id='", PlayerUUID, "' and card_id='", CardUUID, "';"]),
    {ok, 1} = epgsql:squery(Conn, binary_to_list(QuerySetLevel)),

    QuerySetCoins = list_to_binary(["update players set coins=", integer_to_binary(Coins - CoinsRequired)," where player_id='", PlayerUUID, "';"]) ,
    {ok, 1} = epgsql:squery(Conn, binary_to_list(QuerySetCoins)).


update_card_level(Conn, {PlayerUUID, CardUUID}) ->

    {CurrLevel, Frags, Coins, #{frags_required:=FragsRequired, coins_required:=CoinsRequired}} = get_card_info_for_update_level(Conn, {PlayerUUID, CardUUID}),

    if  Frags < FragsRequired ->
            {error, insufficient_frags};
        Coins < CoinsRequired ->
            {error, insufficient_coins};
        true ->
            actual_update_card_level(Conn, {Frags, FragsRequired, Coins, CoinsRequired, PlayerUUID, CardUUID}),
            update_card_skill_points(Conn, {PlayerUUID, CardUUID, 5}),
            {ok, binary_to_integer(CurrLevel)+1, Coins-CoinsRequired, Frags-FragsRequired}
    end.

update_card_skill_points(Conn, {PlayerUUID, CardUUID, SkillPoints}) ->
    QueryUpdate = list_to_binary(["update player_obtained_card set skill_points=skill_points+", integer_to_binary(SkillPoints), " where player_id='", PlayerUUID,"' and card_id='", CardUUID,"' ;"]),
    {ok, 1} = epgsql:squery(Conn, binary_to_list(QueryUpdate) ),

    QueryNew = list_to_binary(["select coins from players where player_id='", PlayerUUID,"';"]),
    {ok, _, [{CoinNew}]} = epgsql:squery(Conn, binary_to_list(QueryNew) ),
    erlang:display(CoinNew).


update_card_skill_level(Conn, {PlayerUUID, CardUUID, SkillName}) ->

    Query = list_to_binary(["select skill_points from player_obtained_card where player_id='", PlayerUUID, "' and card_id='", CardUUID, "'; "]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, _, [{Res}]} ->
            case binary_to_integer(Res) > 0 of
                true ->
                    QueryGetCurrentSkillLevel = list_to_binary(["select skill_level from player_obtained_card_skills
                                                       where player_id='", PlayerUUID, "' and card_id='", CardUUID, "' and skill_name='", SkillName, "'; "]),

                    {ok, _, [{SkillLevel}]} = epgsql:squery(Conn, binary_to_list(QueryGetCurrentSkillLevel)),

                    case binary_to_integer(SkillLevel) < 3 of
                        true ->

                            QuerySkillLevel = list_to_binary(["update player_obtained_card_skills set skill_level=skill_level+1 
                                                               where player_id='", PlayerUUID, "' and card_id='", CardUUID, "' and skill_name='", SkillName, "'; "]),
                            {ok, 1} = epgsql:squery(Conn, binary_to_list(QuerySkillLevel)),

                            QuerySkillPoint = list_to_binary(["update player_obtained_card set skill_points=skill_points-", SkillLevel," where player_id='", PlayerUUID, "' and card_id='", CardUUID, "'; "]),
                            {ok, 1} = epgsql:squery(Conn, binary_to_list(QuerySkillPoint)),


                            QueryGetRemainingSkillPoint = list_to_binary(["select skill_points from player_obtained_card 
                                                               where player_id='", PlayerUUID, "' and card_id='", CardUUID, "'; "]),
                            {ok, _, [{RemainingSkillPoints}]} = epgsql:squery(Conn, binary_to_list(QueryGetRemainingSkillPoint)),

                            {ok, binary_to_integer(SkillLevel)+1, binary_to_integer(RemainingSkillPoints)};
                        _ ->
                            {error, highest_level_reached}
                    end;
                _ ->
                    {error, not_enough_skill_points}
            end;

        {error, Error} ->
            error_logger:info_report(Error),
            {error, Error}
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


store_battle_record(Conn, Content) ->
    dungeon_query_battle_record:store(Conn, Content).

fetch_battle_record_list(Conn, {SelfID, OppoID}) ->
    dungeon_query_battle_record:fetch_list(Conn, SelfID, OppoID).

fetch_battle_record(Conn, {BattleRecordID}) ->
    dungeon_query_battle_record:fetch(Conn, BattleRecordID).
