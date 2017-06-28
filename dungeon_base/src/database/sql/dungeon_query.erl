-module(dungeon_query).

-author('Yue Marvin Tao').

-export([
    connect/0,
    connect/5,
    close/1,

    login/2,
    add_new_player/2,

    get_player/2,
    get_player_battle/2,
    get_player_list/2,

    get_player_rank/2,

    update_preset/2,
    update_rate/2,
    update_rank/2,
    update_frag/2,
    update_coin/2,
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
    fetch_battle_record/2,

    send_mail/2,
    send_mail_attached/2,
    receive_mail_list/2,
    read_mail/2,   
    reply_mail/2,
    delete_mail/2,
    open_attachment/2
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
%% 玩家档案条目记录（来自player表）

add_player_obtained_card(Conn, PlayerID, CardID) ->
    #{player_card_id:=PlayerCardID} = player_obtained_card:add(Conn, PlayerID, CardID),

    {ok, CardSkills} = card_skill:get(Conn),
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
    
    player_obtained_card_skill:add(Conn, SkillsTobeInserted).


login(Conn, {ID}) ->

    Query = util:get_query(<<"player">>, #{player_id=>ID}),
    {ok, Column, Res} = epgsql:squery(Conn, Query),

    ActualRes = util:get_mapped_record(Column, Res),

    case ActualRes of
        [] ->
            {ok, ID} = add_new_player(Conn, {ID}),
            #{created=>ID};
        _ ->
            #{logged => ok}
    end.


add_new_player(Conn, {NewID}) ->

    erlang:display({new_id_tobe_inserted, NewID}),

    {ok, added} = player:add(Conn, NewID),

    add_player_obtained_card(Conn, NewID, <<"946ae77c-183b-4538-b439-ac9036024676">>),
    add_player_obtained_card(Conn, NewID, <<"1b0cf5e0-2164-46fd-8424-2146fca99fb9">>),
 
    {ok, NewID};

    

add_new_player(Conn, {}) ->
    quickrand:seed(),
    NewID = list_to_binary(uuid:uuid_to_string(uuid:get_v4_urandom())),
    erlang:display({new_id_tobe_inserted, NewID}),

    {ok, added} = player:add(Conn, NewID),

    add_player_obtained_card(Conn, NewID, <<"946ae77c-183b-4538-b439-ac9036024676">>),
    add_player_obtained_card(Conn, NewID, <<"1b0cf5e0-2164-46fd-8424-2146fca99fb9">>),
 
    {ok, NewID}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ------------------------------------------------------------------------
%% 得到玩家列表，仅提供player的信息
%% TODO: 未来将会加上更多限定条件，譬如排名等，来限制获取的玩家数目

get_single_listed_player(Conn, PlayerID) ->

    #{preset_card_id:=PresetCardID}=PlayerResult = player:get(Conn, PlayerID),
    
    PlayerCardInfo = card_detail:get(Conn, PlayerID, PresetCardID),

    maps:remove(skills, maps:merge(PlayerResult, PlayerCardInfo)).

get_player_list(Conn, _) ->

    PlayersResult = player:get(Conn),
    SortedPlayerResult  = lists:sort(fun(#{rating:=RatingA}, #{rating:=RatingB}) -> RatingA < RatingB end, PlayersResult),
    SortedPlayerID      = lists:map(fun(#{player_id:=PlayerID}) -> PlayerID end, SortedPlayerResult),
    SortedPlayers       = lists:map(fun(PlayerID) -> get_single_listed_player(Conn, PlayerID) end, SortedPlayerID),

    {ok, SortedPlayers}.

%% ------------------------------------------------------------------------
%% 得到玩家信息，包含玩家信息，和玩家所持的所有卡牌的具体信息
%% NOTE: 一般情况下主要用于自己

get_player(Conn, {PlayerUUID}) ->
    Player = player:get(Conn, PlayerUUID),
    Res    = player_obtained_card:get(Conn, PlayerUUID),
    Cards  = [ card_detail:get(Conn, PlayerUUID, CardID) || #{card_id:=CardID} <- Res],
    {ok, #{player_profile=>Player, card_profiles=>Cards}}.


get_player_battle(Conn, {PlayerUUID}) ->
    battle_context:get(Conn, PlayerUUID);
get_player_battle(Conn, {PlayerUUID, CardID, SelectedSkills}) ->
    battle_context:get(Conn, PlayerUUID, CardID, SelectedSkills).


%% ------------------------------------------------------------------------
%% 更新玩家的预设技能
update_preset(Conn, {SkillList, SelfCardID, PlayerUUID}) ->

    SkillBinList = [list_to_binary(["\"",SkillName,"\""]) || SkillName <- SkillList],

    Set   = #{preset_card_id=>SelfCardID},
    Cond  = #{player_id=>PlayerUUID},
    Query = util:set_query(<<"player">>, Set, Cond), 

    {ok, 1} = epgsql:squery(Conn, Query),
    
    SetSkill  = #{preset_skill=>SkillBinList},
    CondSkill = #{player_id=>PlayerUUID, card_id=>SelfCardID},
    QuerySkill = util:set_query(<<"player_obtained_card">>, SetSkill, CondSkill),

    {ok, 1} = epgsql:squery(Conn, QuerySkill).

%% ------------------------------------------------------------------------
%% 更新玩家的积分
%% NOTE: 服务器完成，不提供webAPI

get_player_rank(Conn, {PlayerUUID}) ->
    #{ranking:=Rank}=player:get(Conn, PlayerUUID),
    {ok, Rank}.

update_rate(Conn, {Rate, PlayerUUID}) ->
    player:set_rate(Conn, {Rate, PlayerUUID}).

update_coin(Conn, {CoinIncre, PlayerUUID}) ->
    player:set_coin(Conn, {CoinIncre, PlayerUUID}).    

update_frag(Conn, {FragIncre, CardID, PlayerUUID}) ->

    Cards = player_obtained_card:get(Conn, PlayerUUID),
    CardList    = [ID || #{card_id:=ID} <-Cards],

    case lists:member(CardID, CardList) of
        false -> player_obtained_card:add(Conn, {CardID, PlayerUUID});
        _ ->
            error_logger:info_report(FragIncre),
            SetExp = #{frags=> {e, list_to_binary(["frags+", FragIncre])}},
            Cond   = #{player_id=>PlayerUUID, card_id=>CardID},
            Query  = util:set_query(<<"player_obtained_card">>, SetExp, Cond),
            error_logger:info_report(binary_to_list(Query)),
            epgsql:squery(Conn, Query)
    end.

update_rank(Conn, {}) ->
    player:set_rank(Conn, {}).

%% ------------------------------------------------------------------------
%% 更新卡牌

get_card_info_for_update_level(Conn, {PlayerUUID, CardUUID}) ->

    [#{card_level:=CurrLevel, frags:=CurrentFrags}] = player_obtained_card:get(Conn, PlayerUUID, CardUUID),

    #{coins:=CurrentCoins} = player:get(Conn, PlayerUUID),

    #{frags_required:=FragsRequired, coins_required:=CoinsRequired} = card_level_spec:get(Conn, CardUUID, CurrLevel),

    {CurrLevel, CurrentFrags, CurrentCoins, FragsRequired, CoinsRequired}.


actual_update_card_level(Conn, {RemainingFrags, RemainingCoins, PlayerUUID, CardUUID}) ->
    QuerySetLevel = list_to_binary(["update player_obtained_card set card_level=card_level+1, 
                                    frags=", integer_to_binary(RemainingFrags)," where player_id='", PlayerUUID, "' and card_id='", CardUUID, "';"]),
    {ok, 1} = epgsql:squery(Conn, binary_to_list(QuerySetLevel)),

    QuerySetCoins = list_to_binary(["update player set coins=", integer_to_binary(RemainingCoins)," where player_id='", PlayerUUID, "';"]) ,
    {ok, 1} = epgsql:squery(Conn, binary_to_list(QuerySetCoins)).


update_card_level(Conn, {PlayerUUID, CardUUID}) ->

    {CurrLevel, Frags, Coins, FragsRequired, CoinsRequired} = get_card_info_for_update_level(Conn, {PlayerUUID, CardUUID}),

    if  Frags < FragsRequired ->
            {error, insufficient_frags};
        Coins < CoinsRequired ->
            {error, insufficient_coins};
        true ->

            RemainingFrags = Frags - FragsRequired,
            RemainingCoins = Coins - CoinsRequired,


            actual_update_card_level(Conn, {RemainingFrags, RemainingCoins, PlayerUUID, CardUUID}),
            update_card_skill_points(Conn, {PlayerUUID, CardUUID, 5}),
            {ok, CurrLevel+1, RemainingCoins, RemainingFrags}
    end.

update_card_skill_points(Conn, {PlayerUUID, CardUUID, SkillPoints}) ->
    player_obtained_card:set_skill_point(Conn, {PlayerUUID, CardUUID, SkillPoints}).


update_card_skill_level(Conn, {PlayerUUID, CardUUID, SkillName}) ->

    #{skill_points:=SkillPoints} = player_obtained_card:get(Conn, PlayerUUID, CardUUID),
    
    case binary_to_integer(SkillPoints) > 0 of
        true ->

            #{skill_level:=SkillLevel}=player_obtained_card_skill:get(Conn, PlayerUUID, CardUUID, SkillName),

            case binary_to_integer(SkillLevel) < 3 of
                true ->

                    player_obtained_card_skill:set(Conn, #{skill_level=>SkillLevel+1}, PlayerUUID, CardUUID, SkillName),

                    player_obtained_card:set(Conn, #{skillPoints=>SkillPoints - SkillLevel}, #{player_id=>PlayerUUID, card_id=>CardUUID}),

                    #{skill_points:=RemainingSkillPoints} = player_obtained_card:get(Conn, PlayerUUID, CardUUID),

                    {ok, binary_to_integer(SkillLevel)+1, binary_to_integer(RemainingSkillPoints)};
                _ ->
                    {error, highest_level_reached}
            end;
        _ ->
            {error, not_enough_skill_points}
    end.
    


update_quick_battle_counter(Conn, {PlayerUUID}) ->

    #{quick_battle_counter:=QuickBattleCounter} = player:get(Conn, PlayerUUID),

    NewQuickBattleCounter = QuickBattleCounter rem 5 + 1,

    player:set(Conn, #{quick_battle_counter=>NewQuickBattleCounter}, #{player_id=>PlayerUUID}), 

    binary_to_integer(NewQuickBattleCounter).


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



send_mail(Conn, {SenderID, ReceiverID, Content}) ->
    mail_service:send_mail(Conn, SenderID, ReceiverID, Content, []).

send_mail_attached(Conn, {SenderID, ReceiverID, Content, Attachments}) ->
    mail_service:send_mail(Conn, SenderID, ReceiverID, Content, Attachments).

receive_mail_list(Conn, {ReceiverID}) -> 
    mail_service:recv_mail_list(Conn, ReceiverID).

read_mail(Conn, {ReceiverID, MailID}) ->
    mail_service:read_mail(Conn, ReceiverID, MailID).

reply_mail(Conn, {PlayerID, ReceiverID, MailID, Content}) ->
    mail_service:reply_mail(Conn, PlayerID, ReceiverID, MailID, Content).

delete_mail(Conn, {PlayerID, MailID}) ->
    mail_service:delete_mail(Conn, PlayerID, MailID).

open_attachment(Conn, {PlayerID, MailID}) ->
    mail_service:open_attachment(Conn, PlayerID, MailID).
