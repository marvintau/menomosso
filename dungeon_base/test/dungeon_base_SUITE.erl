-module(dungeon_base_SUITE).

-author('Yue Marvin Tao').

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0]).
-export([init_per_group/2, end_per_group/2]).
-export([add_new_player_test/1]).

all() ->
    [{group, dungeon_base_server_group}].

groups() ->
    [{dungeon_base_server_group, [], [add_new_player_test]}].

init_per_group(dungeon_base_server_group, Config) ->
    Args = [{host, "localhost"}, {username, "yuetao"}, {password, "asdsadasd"}, {database, "dungeon"}, {timeout, 100}],
    dungeon_base_server:start(Args),
    Config.

end_per_group(dungeon_base_server_group, _Config)->
    dungeon_base_server:stop().

add_new_player_test(_Config) ->
    {ok, Res} = dungeon_base_server:add_new_player(),
    ct:print(default, 90, "hahaha ~62tp~n", [Res]).

chest(Config) ->
    Conn = ?config(conn, Config),
    PlayerID = ?config(player, Config),

    {ok, CheckRes} = dungeon_base:check_chest_update(Conn, PlayerID),
    ct:print(default, 90, "~62tw~n", [CheckRes]),

    {ok, OpenRes} = dungeon_base:open_chest_update(Conn, PlayerID),

    ct:print(default, 90, "~62tp~n", [OpenRes]).
