%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(battle_request_handler).

-export([init/2]).
-export([content_types_provided/2, content_types_accepted/2]).
-export([allow_missing_posts/2]).
-export([allowed_methods/2]).
-export([handle_post/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, Opts) ->
    {[<<"POST">>], Req, Opts}.

content_types_accepted(Req, State) ->

    {[
        {<<"application/json">>, handle_post}
    ], Req, State}.


% note that the method won't be called since the callback
% specified here will be only called when GET and HEAD request
% being processed.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, handle_post}
    ], Req, State}.


allow_missing_posts(Req, State) ->
    {false, Req, State}.

handle_post(Req, State) ->

    {ReqBody, NextReq} = try cowboy_req:read_body(Req) of
        {ok, ReqBodyRaw, NewReq} ->
            {ReqBodyRaw, NewReq}
    catch
        error:Error ->
            erlang:display(Error),
            {<<"Nah">>, Req}
    end,

    {[{_, Id1}, {_, Id2}, {_, SelfCardID}, {_, Skills}]} = jiffy:decode(ReqBody),

    error_logger:info_report(battle_begins),

    {ok, _} = dungeon_base_sup:query({update_selected_skills, {Skills, SelfCardID, Id1}}),

    {ok, BattleContext1} = dungeon_base_sup:query({get_player_battle, {Id1}}),
    {ok, BattleContext2} = dungeon_base_sup:query({get_player_battle, {Id2}}),

    erlang:display(BattleContext1),

    {log, Log} = battle:start({BattleContext1, BattleContext2}),
    Res = cowboy_req:set_resp_body(jiffy:encode(Log), NextReq),
    {true, Res, State}.
