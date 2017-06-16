%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(battle_request_handler).

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
            {ReqBodyRaw, NewReq}
    catch
        error:Error ->
            erlang:display(Error),
            {<<"Nah">>, Req}
    end,

    {[{_, IdA}, {_, IdB}, {_, SelfCardID}, {_, Skills}]} = jiffy:decode(ReqBody),

    error_logger:info_report(battle_begins),

    {ok, #{player_profile:=#{rating:=RateA}}} = dungeon_base_sup:query({get_player, {IdA}}),
    {ok, #{player_profile:=#{rating:=RateB}}} = dungeon_base_sup:query({get_player, {IdB}}),

    {ok, _} = dungeon_base_sup:query({update_selected_skills, {Skills, SelfCardID, IdA}}),

    {ok, #{player_profile:=#{rating:=RateA, selected_skills:=SelectedSkillsA, preset_card_id:=CardIdA}} } = dungeon_base_sup:query({get_player, {IdA}}),
    {ok, #{player_profile:=#{rating:=RateB, selected_skills:=SelectedSkillsB, preset_card_id:=CardIdB}} } = dungeon_base_sup:query({get_player, {IdB}}),

    {ok, BattleContextA} = dungeon_base_sup:query({get_player_battle, {IdA}}),
    {ok, BattleContextB} = dungeon_base_sup:query({get_player_battle, {IdB}}),

    {log, #{winner:=Winner, loser:=Loser}=Log} = battle:start({BattleContextA, BattleContextB}),



    {ResA, ResB} = case Winner of
        IdA -> {1, 0};
        IdB -> {0, 1}
    end,

    K = 16,

    ExpectA = 1/(1+math:exp(RateB - RateA)),
    ExpectB = 1/(1+math:exp(RateA - RateB)),

    NewRateA = round(RateA + K * (ResA - ExpectA)),
    NewRateB = round(RateB + K * (ResB - ExpectB)),

    {ok, rate_updated} = dungeon_base_sup:query({update_rate, {NewRateA, IdA}}),
    {ok, rate_updated} = dungeon_base_sup:query({update_rate, {NewRateB, IdB}}),
    {ok, rank_updated} = dungeon_base_sup:query({update_rank, {}}),

    {ok, {RankA}} = dungeon_base_sup:query({get_player_rank, {IdA}}),
    {ok, {RankB}} = dungeon_base_sup:query({get_player_rank, {IdB}}),

    RatedLog = Log#{new_rate=>#{IdA=>NewRateA, IdB => NewRateB}, new_rank=>#{IdA=>binary_to_integer(RankA), IdB=>binary_to_integer(RankB)}},

    % Supply = case dungeon_base_sup:query({add_supply, {IdA, round(rand:uniform(3))}}) of
    %     {ok, PlayerID, LootID, SupplyType} -> #{player_id => PlayerID, loot_id=>list_to_binary(LootID), supply_type=> SupplyType, error=> <<"none">>};
    %     {full, _} -> #{error => <<"full">>};
    %     Err -> #{error => Err}
    % end,

    % SuppliedLog = RatedLog#{supply=>Supply},


    EncodedLog = jiffy:encode(RatedLog),
    dungeon_base_sup:query({store_battle_record, {IdA, IdB, CardIdA, CardIdB, SelectedSkillsA, SelectedSkillsB, IdA=:=Winner, EncodedLog}}),
    Res = cowboy_req:set_resp_body(EncodedLog, NextReq),

    Res1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST, OPTIONS">>, Res),
    Res2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type, origin, access-control-request-origin">>, Res1),
    Res3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Res2),

    {true, Res3, State}.
