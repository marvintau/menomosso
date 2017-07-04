-module(enemy).

-export([get/2, add/3]).

get(Conn, SelfID) ->
    Query1 = util:get_query(<<"enemy">>, #{self=>SelfID}),
    {ok, Columns, Res1} = epgsql:squery(Conn, Query1),
    Result1 = util:get_mapped_records(Columns, Res1),

    Result1.

add(Conn, SelfID, EnemyID) ->
    Query = util:add_query(<<"enemy">>, #{self=>SelfID, enemy=>EnemyID}),
    {ok, 1} = epgsql:squery(Conn, Query),

    {ok, added}.
