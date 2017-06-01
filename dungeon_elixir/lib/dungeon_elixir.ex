defmodule DungeonElixir do
  
  use Application

  def build_dispatch_config do
    :cowboy_router.compile([
            {:_, [

                   {"/api/get_card_list",   GetCardList, []},
                   {"/api/update_card",     UpdateCard, []},

                   {"/api/add_new_player",  AddPlayer, []},
                   {"/api/get_player",      GetPlayer, []},
                   {"/api/get_player_list", GetPlayerList, []},

                   {"/api/open_chest",      OpenChest, []},
                   {"/api/check_chest",     CheckChest, []},

                   {"/api/battle_request",  BattleRequest, []}
                  ]}
        ])
  end

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    IO.puts("dungeon begins")

    dispatch_config = build_dispatch_config()
    {:ok, _} = :cowboy.start_http(:http, 100, [{:port, 1337}], [{:env, [{:dispatch, dispatch_config}] }])

    children = [ supervisor(DungeonElixir.Repo, []) ]

    opts = [strategy: :one_for_one, name: Friends.Supervisor]
    Supervisor.start_link(children, opts)
  end

end
