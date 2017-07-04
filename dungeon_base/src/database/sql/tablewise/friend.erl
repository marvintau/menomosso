-module(friend).

-export([get/2, add/3]).

get(Conn, SelfID) ->
    Query1 = util:get_query(<<"friend">>, #{friend_a=>SelfID}),
    {ok, Columns, Res1} = epgsql:squery(Conn, Query1),
    Result1 = util:get_mapped_records(Columns, Res1),

    Query2 = util:get_query(<<"friend">>, #{friend_a=>SelfID}),
    {ok, Columns, Res2} = epgsql:squery(Conn, Query2),
    Result2 = util:get_mapped_records(Columns, Res2),

    Result1 ++ Result2.

add(Conn, SelfID, OtherID) ->
    Query = util:add_query(<<"friend">>, #{friend_a=>SelfID, friend_b=>OtherID}),
    {ok, 1} = epgsql:squery(Conn, Query),

    {ok, added}.
