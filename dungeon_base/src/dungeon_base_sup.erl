%%%-------------------------------------------------------------------
%% @doc dungeon_base top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dungeon_base_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, query/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


query({Op, Args}) ->
    poolboy:transaction(dungeon_peon_pool, fun(Worker) ->
        gen_server:call(Worker, {q, Op, Args})
    end).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->

    {ok, Pools} = application:get_env(dungeon_base, pools),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, Name}}, {worker_module, dungeon_worker}] ++ SizeArgs,
        erlang:display({Name, PoolArgs, WorkerArgs}),
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
