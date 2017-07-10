-module(dungeon_query_supply).

-author('Yue Marvin Tao').

-export([add_new_supply/3, check_supply_remaining_time/2, open_supply/2]).

rand_true_false() ->
	case rand:uniform(2) of
		1 -> <<"true">>;
		_ -> <<"false">>
	end.

add_new_supply(Conn, PlayerUUID, SupplyID) ->

    quickrand:seed(),
    LootID = uuid:uuid_to_string(uuid:get_v4_urandom()),

    {ok, RemainingSupplies} = check_supply_remaining_time(Conn, PlayerUUID),

    QueryAddLoot = list_to_binary([
    	"insert into player_supply_loot(loot_id, player_id, supply_id, acquire_time, open_time, is_opened, buff1, buff2, buff3) values
		('", LootID, "', '", PlayerUUID,"', ", integer_to_binary(SupplyID),", now(), now(), false, ", rand_true_false(),", ", rand_true_false(), ", ", rand_true_false(), ");"
    ]),

    QueryAddLootItems = list_to_binary([
		"insert into loot_list(loot_id, item_id, item_qty)
		select '", LootID, "', item_id, qty from (
		    select distinct on (item_group) item_id, round(random() * (max_qty - min_qty) + min_qty) as qty from (
		        select *, generate_series(1, drop_rate) from supply_spec where supply_id=", integer_to_binary(SupplyID),"
		    ) as gene order by item_group, random()
		) as tem"
	]),

    case length(RemainingSupplies) >= 4 of
    	true ->
    		{full, no_more_space_for_new_supply};
	    _ -> case epgsql:squery(Conn, binary_to_list(QueryAddLoot)) of
	    	{ok, _} -> case epgsql:squery(Conn, binary_to_list(QueryAddLootItems)) of
	    		{ok, _} -> {ok, PlayerUUID, LootID, SupplyID};
	    		Error ->
	    			error_logger:info_report({add_loot_item, Error}),
	    			{error, Error}
	    		end;
                Error ->
                        error_logger:info_report({add_loot, Error}),
                        {error, Error}
	    end
	end.

check_supply_remaining_time(Conn, PlayerUUID) ->
	Query = list_to_binary(["select loot_id, tem.supply_id supply_type, buff1, buff2, buff3, date_part('epoch', interval '1s' * cooldown_time - (now()-acquire_time)) * interval '1s' as remaining
		from supply_name join (select loot_id, acquire_time, supply_id, buff1, buff2, buff3 from player_supply_loot where player_id='", PlayerUUID, "' and is_opened='f') as tem on tem.supply_id=supply_name.supply_id;"]),

	{ok, _, Res} = epgsql:squery(Conn, binary_to_list(Query)),
	{ok, Res}.


return_supply_items(Conn, LootID) ->
	QueryResult = list_to_binary(["select loot_id, loot.item_id, item_qty, item_name, description from
		(select * from loot_list where loot_id='", LootID, "') as loot
		join supply_items on supply_items.item_id=loot.item_id;"
	]) ,

	{ok, _, Res} = epgsql:squery(Conn, binary_to_list(QueryResult)),
	{ok, Res}.


fetch_supply_items(Conn, PlayerID, {_, ItemID, ItemQty, _, _}) ->
	case ItemID of
		<<"1">> ->
			Res=dungeon_query:update_coin(Conn, {binary_to_integer(ItemQty), PlayerID}),
			erlang:display(Res);
		<<"2">> ->dungeon_query:update_frag(Conn, {ItemQty, <<"946ae77c-183b-4538-b439-ac9036024676">>, PlayerID});
		<<"3">> ->dungeon_query:update_frag(Conn, {ItemQty, <<"1b0cf5e0-2164-46fd-8424-2146fca99fb9">>, PlayerID});
		<<"4">> ->dungeon_query:update_frag(Conn, {ItemQty, <<"a009e5e9-2057-4353-9871-309d68752c1b">>, PlayerID});
		<<"5">> ->dungeon_query:update_frag(Conn, {ItemQty, <<"a0c1a883-2995-4526-856c-26870e5b3f74">>, PlayerID});
		_ ->
			not_implemented_yet
	end.


open_supply(Conn, LootID) ->
	
	Query = list_to_binary(["select is_opened from player_supply_loot where loot_id='", LootID, "';
		update player_supply_loot set is_opened='t', open_time=now() where loot_id='", LootID, "';
		select player_id, supply_id from player_supply_loot where loot_id='", LootID,"';"
	]),

        case epgsql:squery(Conn, binary_to_list(Query)) of 
	    [{ok, _, [{IsOpened}]}, {ok, 1}, {ok, _, [{PlayerID, SupplyID}]}] ->

                case IsOpened of
                        <<"t">> ->
                                supply_has_been_opened;
                        _ ->
                                {ok, Res} = return_supply_items(Conn, LootID),
                                [ fetch_supply_items(Conn, PlayerID, Item) || Item <- Res],
                                error_logger:info_report(Res),
                                {ok, Res}
                end;
            Error ->
                invalid_supply_id
        end.

