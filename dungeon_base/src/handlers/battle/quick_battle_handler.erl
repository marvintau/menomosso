%% Feel free to use, reuse and abuse the code in this file.

-module(quick_battle_handler).

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

    {[{_, IdA}]} = jiffy:decode(ReqBody),

    {ok, List} = dungeon_base_sup:query({get_player_list, {}}),
    erlang:display({show_id, IdA}),
    IdB = lists:nth(rand:uniform(length(List)-1), lists:delete(IdA, [maps:get(player_id, Player) || Player <- List])),
    erlang:display({show_id, IdB}),


    error_logger:info_report(battle_begins),


    {ok, #{card_profiles:=CardsA, player_profile:=#{rating:=RateA, selected_skills:=SelectedSkillsA, preset_card_id:=CardIdA}} = PlayerA} = dungeon_base_sup:query({get_player, {IdA}}),
    {ok, #{card_profiles:=CardsB, player_profile:=#{rating:=RateB, selected_skills:=SelectedSkillsB, preset_card_id:=CardIdB}} = PlayerB} = dungeon_base_sup:query({get_player, {IdB}}),


    PlayerAWithCards = PlayerA#{card => hd([CardA || CardA <- CardsA, maps:get(card_id, CardA) =:= CardIdA ])},
    PlayerAWithCardProfilesRemoved = maps:remove(card_profiles, PlayerAWithCards),

    PlayerBWithCards = PlayerB#{card => hd([CardB || CardB <- CardsB, maps:get(card_id, CardB) =:= CardIdB ])},
    PlayerBWithCardProfilesRemoved = maps:remove(card_profiles, PlayerBWithCards),

    {ok, BattleContextA} = dungeon_base_sup:query({get_player_battle, {IdA}}),
    {ok, BattleContextB} = dungeon_base_sup:query({get_player_battle, {IdB}}),

    error_logger:info_report(BattleContextA),

    {log, #{winner:=Winner, loser:=Loser}=Log} = battle:start({BattleContextA, BattleContextB}),

    EncodedLog = jiffy:encode(Log),

    dungeon_base_sup:query({store_battle_record, {IdA, IdB, CardIdA, CardIdB, SelectedSkillsA, SelectedSkillsB, IdA=:=Winner, EncodedLog}}),

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

    %Supply = case IdA == Winner of
        %true ->
            %erlang:display("===================== WIN WIN WIN"),
            %case dungeon_base_sup:query({update_quick_battle_counter, {IdA}}) of
                %0 ->
                    %erlang:display(ok_to_get_supply),
                    %case dungeon_base_sup:query({add_supply, {IdA, round(rand:uniform(3))}}) of
                        %{ok, PlayerID, LootID, SupplyType} -> #{player_id => PlayerID, loot_id=>list_to_binary(LootID), supply_type=> SupplyType, error=> <<"none">>};
                        %{full, _} -> #{error => <<"full">>};
                        %Err -> #{error => Err}
                    %end;
                %A ->
                    %erlang:display({how_many, A}),
                    %#{error => <<"not_enough_win">>}
            %end;
        %_ ->
            %erlang:display(ooops_loose),
            %#{error => <<"lose">>}
    %end,

     Supply = case dungeon_base_sup:query({add_supply, {IdA, round(rand:uniform(3))}}) of
         {ok, PlayerID, LootID, SupplyType} -> #{player_id => PlayerID, loot_id=>list_to_binary(LootID), supply_type=> SupplyType, error=> <<"none">>};
         {full, _} -> #{error => <<"full">>};
         Err -> #{error => Err}
     end,

    SuppliedLog = RatedLog#{supply=>Supply, player_self=>PlayerAWithCardProfilesRemoved, player_oppo=>PlayerBWithCardProfilesRemoved},

    Res = cowboy_req:set_resp_body(jiffy:encode(SuppliedLog), NextReq),

    Res1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST, OPTIONS">>, Res),
    Res2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type, origin, access-control-request-origin">>, Res1),
    Res3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Res2),

    {true, Res3, State}.
