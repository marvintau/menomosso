-module(dungeon_base_SUITE).

-author('Yue Marvin Tao').

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0]).
-export([init_per_group/2, end_per_group/2]).
-export([add_new_player/1, add_new_card/1]).

all() ->
    [{group, dungeon_group}].

groups() ->
    [{dungeon_group, [], [add_new_player, add_new_card]}].

init_per_group(dungeon_group, Config) ->
    Args = [{host, "localhost"}, {username, "yuetao"}, {password, "asdsadasd"}, {database, "dungeon"}, {timeout, 100}],
    dungeon_base:start(Args),
    Config.

end_per_group(dungeon_group, _Config)->
    dungeon_base:stop().

add_new_player(_Config) ->
    {ok, Res} = dungeon_base:add_new_player(),
    ct:print(default, 90, "added new player ~62tp~n", [Res]).

add_new_card(_Config) ->
    {ok, Res} = dungeon_base:add_new_card(),
    ct:print(default, 90, "added new card ~62tp~n", [Res]).
