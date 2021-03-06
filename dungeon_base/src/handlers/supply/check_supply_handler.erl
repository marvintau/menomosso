-module(check_supply_handler).

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
        {<<"application/text">>, handle_post},
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
            error_logger:info_report({request, error, Error}),
            {<<"Nah">>, Req}
    end,

    {[{_, PlayerID}]} = jiffy:decode(ReqBody),

    Payload = case dungeon_base_sup:query({check_supply,{PlayerID}}) of
        {ok, QueryRes} ->
            erlang:display(QueryRes),
            [#{loot_id=> LootID, supply_type=>SupplyType, buff1=>Buff1, buff2=>Buff2, buff3=>Buff3, remaining_time=> RemainingTime} || {LootID, SupplyType, Buff1, Buff2, Buff3, RemainingTime} <- QueryRes];
        Err -> #{error => Err}
    end,

    Res = cowboy_req:set_resp_body(jiffy:encode(Payload), NextReq),
    {true, Res, State}.
