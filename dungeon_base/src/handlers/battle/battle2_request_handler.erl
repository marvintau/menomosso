%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(battle2_request_handler).

-export([init/2]).
-export([content_types_provided/2, content_types_accepted/2]).
-export([allow_missing_posts/2]).
-export([allowed_methods/2]).
-export([options/2]).
-export([handle_post/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, Opts) ->
    {[<<"POST">>, <<"OPTIONS">>], Req, Opts}.

options(Req, State) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST, OPTIONS">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type, origin, access-control-request-origin">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req2),
    {ok, Req3, State}.

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
            erlang:display(binary_to_list(ReqBodyRaw)),
            {ReqBodyRaw, NewReq}
    catch
        error:Error ->
            erlang:display(Error),
            {<<"Nah">>, Req}
    end,

    #{a:=#{id:=IDA, card:=CardA, skills:=SkillsA}, b:=#{id:=IDB, card:=CardB, skills:=SkillsB}} = jiffy:decode(ReqBody, [return_maps]),

    error_logger:info_report(battle_begins),

    {ok, BattleContextA} = dungeon_base_sup:query({get_player_battle, {IDA, CardA, SkillsA}}),
    {ok, BattleContextB} = dungeon_base_sup:query({get_player_battle, {IDB, CardB, SkillsB}}),

    {log, Log} = battle:start({BattleContextA, BattleContextB}),

    EncodedLog = jiffy:encode(Log),
    Res = cowboy_req:set_resp_body(EncodedLog, NextReq),

    Res1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST, OPTIONS">>, Res),
    Res2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type, origin, access-control-request-origin">>, Res1),
    Res3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Res2),

    {true, Res3, State}.