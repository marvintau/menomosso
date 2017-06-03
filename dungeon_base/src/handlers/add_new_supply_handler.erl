-module(add_new_supply_handler).

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

    {[{<<"player_id">>, PlayerID}]} = jiffy:decode(ReqBody),

    Payload = case dungeon_base_sup:query({add_supply,{PlayerID, round(rand:uniform(3))}}) of
        {ok, PlayerID, LootID, SupplyType} -> #{player_id => PlayerID, loot_id=>list_to_binary(LootID), supply_type=> SupplyType, error=> <<"none">>};
        {full, _} -> #{error => <<"full">>};
        Err -> #{error => Err}
    end,

    Res = cowboy_req:set_resp_body(jiffy:encode(Payload), NextReq),
    {true, Res, State}.