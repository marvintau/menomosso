-module(dungeon_worker).

-author('Yue Marvin Tao').

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
    start/1,
    start_link/1,
    stop/0,

    query/1
]).


start(Args) ->
    gen_server:start(?MODULE, Args, []).

start_link(Args) ->
    dungeon_sup:start_link(),
    gen_server:start_link(?MODULE, Args, []).

stop() ->
    gen_server:call(?MODULE, stop).


query({QueryOp, Args}) ->
    gen_server:call(?MODULE, {q, QueryOp, Args}).


init(Args)->
    process_flag(trap_exit, true),
    {ok, Conn} = dungeon_query:connect(),
    {ok, #{conn => Conn}}.

handle_call({q, Operation, Args}, _From, #{conn:=Conn}=State) ->
    {reply, dungeon_query:Operation(Conn, Args), State};

handle_call(stop, _From, #{conn:=Conn} = State) ->
    dungeon_query:close(Conn),
    {stop, normal, user_terminates, State}.

handle_cast(_, _) -> ok.

handle_info(_, _) -> ok.

terminate(_, _) ->
    % dungeon_query:close(Conn),
    ok.

code_change(_, _, _) -> ok.
