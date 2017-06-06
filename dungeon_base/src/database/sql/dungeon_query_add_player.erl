-module(dungeon_query_add_player).
-author('Yue Marvin Tao').


-export([add_player/1, add_player_card/3]).

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

add_player_profile(Conn, PlayerUUID, PlayerName) ->
    Query = list_to_binary(["insert into players values(
    '", PlayerUUID, "', '", PlayerName,"', '", integer_to_binary(round(rand:uniform())) ,"', 'league', 500, 1, 100, 100,
    '946ae77c-183b-4538-b439-ac9036024676',
    '{\"single_attack\", \"single_attack\", \"single_attack\", \"single_attack\", \"single_attack\",
      \"single_attack\", \"single_attack\", \"single_attack\", \"single_attack\", \"single_attack\"}',
    1000.5, 1, now(), now()
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

add_player_card(Conn, CardUUID, PlayerUUID) ->
    Query = list_to_binary(["insert into player_card_info values
    (uuid_generate_v4(), '", CardUUID, "', '", PlayerUUID, "', 1, now(), now()), 1, 1, 0;"]),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, 1} -> {ok, new_card_added};
        _ -> {error, add_card_failed}
    end.


add_player(Conn) ->
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

    {ok, new_player_added} = add_player_profile(Conn, NewID, CheckedName),

    {ok, new_card_added} = add_player_card(Conn, '946ae77c-183b-4538-b439-ac9036024676', NewID),
    {ok, new_card_added} = add_player_card(Conn, 'a0c1a883-2995-4526-856c-26870e5b3f74', NewID),
    {ok, new_card_added} = add_player_card(Conn, 'a009e5e9-2057-4353-9871-309d68752c1b', NewID),
    {ok, new_card_added} = add_player_card(Conn, '1b0cf5e0-2164-46fd-8424-2146fca99fb9', NewID),
    
    {ok, new_chest_record_created} = add_chest_record(Conn, NewID),

    {ok, NewID}.
