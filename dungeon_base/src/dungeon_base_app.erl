%%%-------------------------------------------------------------------
%% @doc dungeon_base public API
%% @end
%%%-------------------------------------------------------------------

-module(dungeon_base_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0, stop/0]).
% -export([init/1, query/1]).

%%====================================================================
%% API
%%====================================================================

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
            {'_', [

                  {"/api/get_card_list", get_card_list_handler, []},
                  {"/api/update_card", update_card_handler, []},

                  {"/api/login", login_handler, []},
                   {"/api/add_new_player", add_new_player_handler, []},
                   {"/api/get_player", get_player_handler, []},
                   {"/api/get_player_list", get_player_list_handler, []},

                   {"/api/open_chest", open_chest_handler, []},
                   {"/api/check_chest", check_chest_handler, []},

                   {"/api/battle_request", battle_request_handler, []},
                   {"/api/test", battle2_request_handler, []},
                   {"/api/quick_battle", quick_battle_handler, []},

                   {"/api/check_supply", check_supply_handler, []},
                   {"/api/open_supply", open_supply_handler, []},

                   {"/api/update_card_level", update_card_level_handler, []},
                   {"/api/update_card_skill_level", update_card_skill_handler, []},

                   {"/api/update_preset", update_preset_handler, []},

                   {"/api/send_mail", send_mail_handler, []},
                   {"/api/send_mail_attachment", send_mail_attachment_handler, []},
                   {"/api/receive_mail_list", receive_mail_list_handler, []},
                   {"/api/receive_mail", receive_mail_handler, []},
                   {"/api/reply_mail", reply_mail_handler, []},
                   {"/api/delete_mail", delete_mail_handler, []},
                   {"/api/open_attachment", open_attachment_handler, []},

                   {"/api/send_friend_request", send_friend_request_handler, []},
                   {"/api/accept_friend_request", accept_friend_request_handler,[]},
                   {"/api/reject_friend_request", reject_friend_request_handler,[]},
                   {"/api/get_friend_list", get_friend_list_handler, []},
                   {"/api/get_circle", get_circle_handler, []},
                   {"/api/post_circle", post_circle_handler, []},
                   {"/api/reply_circle", reply_circle_handler, []},
                   {"/api/delete_circle", delete_circle_handler, []},
                   {"/api/delete_circle_reply", delete_circle_reply_handler, []},
                   {"/api/up_circle", up_circle_handler, []},
                   {"/api/cancel_up_circle", cancel_up_circle_handler, []}

                  ]}
        ]),

    {ok, _} = cowboy:start_clear(http,
        [{port, 1337}],
        #{env => #{dispatch => Dispatch}}
    ),

    ok = skills:init_table(),

    dungeon_base_sup:start_link().

stop(_State) ->
    ok.

%%--------------------------------------------------------------------

%%====================================================================
%% Internal functions
%%====================================================================
