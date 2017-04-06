%%%-------------------------------------------------------------------
%% @doc dungeon_base public API
%% @end
%%%-------------------------------------------------------------------

-module(dungeon_base_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->

    Dispatch = cowboy_router:compile([
            {'_', [
                   {"/api/add_new_player", add_new_player_handler, []},
                   {"/api/get_player", get_player_handler, []},
                   {"/api/get_player_list", get_player_list_handler, []},

                   {"/api/open_chest", open_chest_handler, []},
                   {"/api/check_chest", check_chest_handler, []},

                   {"/api/battle_request", battle_request_handler, []}
                  ]}
        ]),

    {ok, _} = cowboy:start_clear(my_http_listener, 100,
        [{port, 1337}],
        #{env => #{dispatch => Dispatch}}
    ),

	skills:init_table(),

  dungeon_base:start_link([]),

  dungeon_base_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
