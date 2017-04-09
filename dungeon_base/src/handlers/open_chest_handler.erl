-module(open_chest_handler).

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
% specified here will be only called when GET and HEAD reQuery
% being processed.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, handle_post}
    ], Req, State}.


allow_missing_posts(Req, State) ->
    {false, Req, State}.


handle_post(Req, State) ->

    error_logger:info_report(open_chest_check),

    {ReqBody, NextReq} = try cowboy_req:read_body(Req) of
        {ok, ReqBodyRaw, NewReq} ->
            {ReqBodyRaw, NewReq}
    catch
        error:Error ->
            erlang:display(Error),
            {<<"Nah">>, Req}
    end,

    {[{_, ID}]} = jiffy:decode(ReqBody),

    {ok, OpenChestResult} = dungeon_base:query({open_chest_update, {ID}}),
    Result = case is_list(OpenChestResult) of
        true -> [#{id => binary_to_integer(ItemIndex), name=> ItemName, quantity => binary_to_integer(ItemQuantity)} || {ItemIndex, ItemName, ItemQuantity} <- OpenChestResult];
        _ -> atom_to_binary(OpenChestResult, utf8)
    end,

    Res = cowboy_req:set_resp_body(jiffy:encode(Result), NextReq),
    {true, Res, State}.
