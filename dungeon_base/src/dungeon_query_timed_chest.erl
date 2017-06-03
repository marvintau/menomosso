-module(dungeon_query_timed_chest).

-author('Yue Marvin Tao').

-export([check_chest_and_update/2, open_chest_and_update/2]).

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


check_chest_and_update(Conn, PlayerID) ->
    {ok, CheckResult, IsSameDay} = check_chest(Conn, PlayerID),

    case IsSameDay of
        <<"f">> ->
            {ok, next_day_reset} = next_day_reset(Conn, PlayerID),
            {ok, NewCheckResult, _IsSameDay} = check_chest(Conn, PlayerID),
            {ok, NewCheckResult};
        _ ->
            {ok, check_chest_to_map(CheckResult)}
    end.

open_chest_and_update(Conn, PlayerID) ->
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

