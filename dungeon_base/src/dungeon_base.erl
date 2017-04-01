-module(dungeon_base).

-author('Yue Marvin Tao').

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
    start/1,
    start_link/1,
    stop/0,

    add_new_player/0,
    add_new_card/0,
    add_player_card/2,

    get_player/1,
    get_player_battle/1,
    get_player_list/0,
    get_card/1,
    get_card_battle/1,

    update_preset_card/2,
    update_selected_skills/2,
    update_player_ranking/2,
    update_player_level/2,

    check_chest/1,
    open_chest/1
]).


start(Args) ->
    dungeon_base_sup:start_link(),
    gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop() ->
    gen_server:call(?MODULE, stop).



add_new_player() ->
    gen_server:call(?MODULE, {q, add_new_player, {}}).

add_new_card() ->
    gen_server:call(?MODULE, {q, add_new_card, {}}).

get_player_list() ->
    gen_server:call(?MODULE, {q, get_player_list, {}}).




add_player_card(CardID, PlayerID) ->
    gen_server:call(?MODULE, {q, add_player_card, {CardID, PlayerID}}).


get_player(PlayerID) ->
    gen_server:call(?MODULE, {q, get_player, {PlayerID}}).

get_player_battle(PlayerID) ->
    gen_server:call(?MODULE, {q, get_player_battle, {PlayerID}}).


get_card(CardID) ->
    gen_server:call(?MODULE, {q, get_card, {CardID}}).


get_card_battle(CardID) ->
    gen_server:call(?MODULE, {q, get_card_battle, {CardID}}).


update_preset_card(CardID, PlayerID) ->
    gen_server:call(?MODULE, {q, update_preset_card, {CardID, PlayerID}}).

update_selected_skills(Skills, PlayerID) ->
    gen_server:call(?MODULE, {q, update_selected_skills, {Skills, PlayerID}}).

update_player_ranking(Ranking, PlayerID) ->
    gen_server:call(?MODULE, {q, update_player_ranking, {Ranking, PlayerID}}).

update_player_level(Level, PlayerID) ->
    gen_server:call(?MODULE, {q, update_player_level, {Level, PlayerID}}).



check_chest(PlayerID) ->
    gen_server:call(?MODULE, {q, check_chest_update, {PlayerID}}).

open_chest(PlayerID) ->
    gen_server:call(?MODULE, {q, open_chest_update, {PlayerID}}).



init(Args)->
    process_flag(trap_exit, true),

    Host = proplists:get_value(host, Args),
    User = proplists:get_value(username, Args),
    Password = proplists:get_value(password, Args),
    Database = proplists:get_value(database, Args),
    Timeout = proplists:get_value(timeout, Args),


    Res = case dungeon_query:connect(Host, User, Password, Database, Timeout) of
        {ok, Conn} ->
            erlang:display({'DungenBase', connected}),
            {ok, #{conn=>Conn}};
        {error, Error} ->
            erlang:display({'DungenBase', connection, failed}),
            {error, Error}
    end,

    Res.

handle_call({q, Operation, Args}, _From, #{conn:=Conn}=State) ->
    {reply, dungeon_query:Operation(Conn, Args), State};

handle_call(stop, _From, State) ->
    {stop, normal, user_terminates, State}.

handle_cast(_, _) -> ok.

handle_info(_, _) -> ok.

terminate(_, _) -> ok.

code_change(_, _, _) -> ok.
