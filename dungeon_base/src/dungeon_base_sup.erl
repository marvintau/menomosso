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

    PoolSpecs = [ poolboy:child_spec(
                    dungeon_peon_pool,
                    [
                     {name, {local, dungeon_peon_pool}},
                     {worker_module, dungeon_worker},
                     {size, 20},
                     {max_overflow, 50}
                    ],
                    [])
                ],
    {ok, {#{strategy=>one_for_one, intensity=>1, period=>3}, PoolSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
