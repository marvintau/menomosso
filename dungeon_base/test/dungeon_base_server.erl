-module(dungeon_base_server).

-author('Yue Marvin Tao').

% -behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
    start/1,
    stop/0,

    add_new_player/0,
    add_player_card/2,

    get_player/1,
    get_player_list/0,
    get_card/1,

    update_preset_card/2,
    update_preset_skills/2,
    update_player_ranking/2,
    update_player_level/2,

    check_chest/1,
    open_chest/1
]).


start(Args) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

stop() ->
    gen_server:call(?MODULE, stop).

add_new_player() ->
    gen_server:call(?MODULE, {add_new_player}).

add_player_card(CardID, PlayerID) ->
    gen_server:call(?MODULE, {add_player_card, CardID, PlayerID}).


get_player(PlayerID) ->
    gen_server:call(?MODULE, {get_player, PlayerID}).

get_player_list() ->
    gen_server:call(?MODULE, {get_player_list}).

get_card(CardID) ->
    gen_server:call(?MODULE, {get_card, CardID}).



update_preset_card(CardID, PlayerID) ->
    gen_server:call(?MODULE, {update_preset_card, CardID, PlayerID}).

update_preset_skills(Skills, PlayerID) ->
    gen_server:call(?MODULE, {update_preset_skills, Skills, PlayerID}).

update_player_ranking(Ranking, PlayerID) ->
    gen_server:call(?MODULE, {update_player_ranking, Ranking, PlayerID}).

update_player_level(Level, PlayerID) ->
    gen_server:call(?MODULE, {update_player_level, Level, PlayerID}).



check_chest(PlayerID) ->
    gen_server:call(?MODULE, {check_chest, PlayerID}).

open_chest(PlayerID) ->
    gen_server:call(?MODULE, {open_chest, PlayerID}).



init(Args)->
    process_flag(trap_exit, true),

    Host = proplists:get_value(host, Args),
    User = proplists:get_value(username, Args),
    Password = proplists:get_value(password, Args),
    Database = proplists:get_value(database, Args),
    Timeout = proplists:get_value(timeout, Args),

    case dungeon_base_worker:connect(Host, User, Password, Database, Timeout) of
        {ok, Conn} ->
            error_logger:info_report("DungenBase connected."),
            {ok, #{conn=>Conn}};
        {error, Error} ->
            error_logger:info_report("DungenBase connection failed."),
            {error, Error}
    end.

handle_call({add_new_player}, _From, #{conn:=Conn}=State) ->
    {reply, dungeon_base_worker:add_new_player(Conn), State};

handle_call({get_player, PlayerID}, _From, State) ->
    {reply, dungeon_base_worker:get_player(PlayerID), State};

handle_call({get_player_list}, _From, State) ->
    {reply, dungeon_base_worker:get_player_list(), State};

handle_call({update_preset_card, CardID, PlayerID}, _From, State) ->
	{reply, dungeon_base_worker:update_preset_card(CardID, PlayerID), State};

handle_call({update_preset_skills, Skills, PlayerID}, _From, State) ->
	{reply, dungeon_base_worker:update_preset_skills(Skills, PlayerID), State};

handle_call({update_player_ranking, Ranking, PlayerID}, _From, State) ->
	{reply, dungeon_base_worker:update_player_ranking(Ranking, PlayerID), State};

handle_call({update_player_level, Level, PlayerID}, _From, State) ->
	{reply, dungeon_base_worker:update_player_level(Level, PlayerID), State};

handle_call({add_player_card, CardID, PlayerID}, _From, #{conn:=Conn}=State) ->
    {reply, dungeon_base_worker:add_new_player(Conn, CardID, PlayerID), State};

handle_call({check_chest, PlayerID}, _From, State) ->
    {reply, dungeon_base_worker:check_chest(PlayerID), State};

handle_call({open_chest, PlayerID}, _From, State) ->
    {reply, dungeon_base_worker:open_chest(PlayerID), State};

handle_call(stop, _From, State) ->
    {stop, normal, user_terminates, State}.

handle_cast(_, _) -> ok.

handle_info(_, _) -> ok.

terminate(_, _) -> ok.

code_change(_, _, _) -> ok.
