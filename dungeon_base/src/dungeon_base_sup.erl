%%%-------------------------------------------------------------------
%% @doc dungeon_base top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dungeon_base_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, Pools} = application:get_env(dungeon_base, pools),
    PoolSpec = lists:map(fun ({PoolName, SizeArgs, WorkerArgs}) ->
                             PoolArgs = [{name, {local, PoolName}},
                                         {worker_module, dungeon_base_worker}] ++ SizeArgs,
                             poolboy:child_spec(PoolName, PoolArgs, WorkerArgs)
                         end, Pools),
    {ok, { {one_for_one, 10, 10}, PoolSpec} }.

add_pool(Name, PoolArgs, WorkerArgs) ->
    ChildSpec = poolboy:child_spec(Name, PoolArgs, WorkerArgs),
    supervisor:start_child(?MODULE, ChildSpec).
%%====================================================================
%% Internal functions
%%====================================================================
