-module(player).

-export([add/2, get/1, get/2]).
-export([set_rate/2, set_coin/2, set_rank/2]).

random_name() ->
    SurNames = [
         <<"威猛的"/utf8>>, <<"霸道的"/utf8>>, <<"机智的"/utf8>>, <<"勇敢的"/utf8>>, <<"无敌的"/utf8>>,
         <<"可爱的"/utf8>>, <<"呆萌的"/utf8>>, <<"天然的"/utf8>>, <<"干练的"/utf8>>, <<"飒爽的"/utf8>>
    ],
    GivenNames = [
        <<"总裁"/utf8>>,   <<"战士"/utf8>>, <<"魔法使"/utf8>>, <<"隐士"/utf8>>, <<"高人"/utf8>>,
        <<"科学家"/utf8>>, <<"学霸"/utf8>>, <<"天才"/utf8>>,   <<"学生会长"/utf8>>, <<"体育部长"/utf8>>
    ],

    Surname = lists:nth(rand:uniform(10), SurNames),
    Given =   lists:nth(rand:uniform(10), GivenNames),

    list_to_binary([Surname, Given]).


num_duplicate_names(Conn, PlayerName) ->
    Query = list_to_binary(["select * from player where player_name like '", PlayerName, "%'"]),
    {ok, _, Res} = epgsql:squery(Conn, binary_to_list(Query)),
    
    case length(Res) of
        0     -> <<"">>;
        Other -> integer_to_binary(Other)
    end.

get_valid_name(Conn) ->
    
    PlayerName    = random_name(),
    Number        = num_duplicate_names(Conn, PlayerName),
    NewPlayerName = list_to_binary([PlayerName, Number]),
    NewPlayerName. 



%% ------------------------------------------------------------------------
%% 向player表内添加一个新的玩家条目
%% NOTE: 不要单独export

add(Conn, PlayerUUID) ->

    PlayerName = get_valid_name(Conn),

    Query = util:add_query(<<"player">>, #{player_name=>PlayerName, player_id=>PlayerUUID}),
    {ok, 1} = epgsql:squery(Conn,binary_to_list(Query)),
    {ok, added}.

get(Conn, PlayerUUID) ->

    Query = util:get_query(<<"player">>, #{player_id=>PlayerUUID}),
    {ok, ColumnSpec, Result} = epgsql:squery(Conn, binary_to_list(Query)),
    Res = util:get_mapped_records(ColumnSpec, Result),
    
    case Res of
        [] -> #{error=><<"player not found">>};
        [Res] -> Res
    end.

get(Conn) ->
    Query = util:get_query(<<"player">>),
    {ok, ColumnSpec, Result} = epgsql:squery(Conn, binary_to_list(Query)),
    Res = util:get_mapped_records(ColumnSpec, Result),
    Res.

set_rate(Conn, {Rate, PlayerUUID}) ->

    Query = util:set_query(<<"player">>, #{rating=>Rate}, #{player_id=>PlayerUUID}),

    case epgsql:squery(Conn, binary_to_list(Query)) of
        {ok, 1} -> {ok, rate_updated};
        Error       ->
            error_logger:info_report(Error),
            {error, update_rate_failed}
    end.

set_coin(Conn, {CoinIncre, PlayerUUID}) ->
    
    SetExp = #{coins=> {e, list_to_binary(["coins+", integer_to_binary(CoinIncre)])}},
    Query  = util:set_query(<<"player">>, SetExp, #{player_id=>PlayerUUID}),
    epgsql:squery(Conn, Query).

set_rank(Conn, {}) ->
    Query = "update player set ranking=row_number from 
                (select player_id, rating, row_number() over (order by rating desc) from player)
                temp where player.player_id=temp.player_id;",

    case epgsql:squery(Conn, Query) of
        {ok, _} -> {ok, rank_updated};
        Error   ->
            error_logger:info_report(Error),
            {error, update_rank_failed}
    end.


