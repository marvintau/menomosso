-module(ref_test_server).

-author('Yue Marvin Tao').

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
    start/1,
    stop/0,

    val/1, set/2, trans/2
]).


start(Args) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

stop() ->
    gen_server:call(?MODULE, stop).



val(Desc) ->
    gen_server:call(?MODULE, {val, Desc}).

set(Desc, Val) ->
    gen_server:call(?MODULE, {set, {Desc, Val}}).

trans(Op ,Ref) ->
    gen_server:call(?MODULE, {trans, {Op, Ref}}).

init(Args)->
    process_flag(trap_exit, true),

    {ok, List} = dungeon_base:get_player_list(),

    #{preset_card_id:=OffCardID} = OffPlayer = lists:nth(1, List),
    #{preset_card_id:=DefCardID} = DefPlayer = lists:nth(2, List),

    {ok, OffCard} = dungeon_base:get_card_battle(OffCardID),
    {ok, DefCard} = dungeon_base:get_card_battle(DefCardID),

    {ok, #{off=>maps:merge(OffPlayer, OffCard), def=>maps:merge(DefPlayer, DefCard)}}.


handle_call({trans, {Op, Ref}}, _From, #{off:=Off, def:=Def} = State) ->
    {reply, trans:trans({Op, Ref}, Off, Def), State};


handle_call({set, {Desc, Val}}, _From, #{off:=Off, def:=Def} = State) ->
    {reply, ref:set(Desc, Off, Def, Val), State};

handle_call({val, Desc}, _From, #{off:=Off, def:=Def} = State) ->
    {reply, ref:val(Desc, Off, Def), State};


handle_call(stop, _From, State) ->
    {stop, normal, user_terminates, State}.

handle_cast(_, _) -> ok.

handle_info(_, _) -> ok.

terminate(Message, _State) ->
    erlang:display({?MODULE, terminated, Message}),
    ok.

code_change(_, _, _) -> ok.
