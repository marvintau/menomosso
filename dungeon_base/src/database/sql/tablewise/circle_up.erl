-module(circle_up).

-export([get/2, add/3, cancel/4]).

get(Conn, CircleID) ->
    Query1 = util:get_query(<<"circle_reply">>, #{circle_id=>CircleID}),
    {ok, Columns, Res1} = epgsql:squery(Conn, Query1),
    Result1 = util:get_mapped_records(Columns, Res1),

    Result1.

add(Conn, CircleID, PlayerID) ->
    Query = util:add_query(<<"circle_reply">>, #{circle_id=>CircleID, player_id=>PlayerID}),
    {ok, 1} = epgsql:squery(Conn, Query),

    #{ok=>added}.

cancel(Conn, CircleID, CircleUpID, PlayerID) ->

    Query = util:get_query(<<"circle_up">>, #{circle_id=>CircleID, circle_up_id=>CircleUpID}),
    {ok, Columns, Res} = epgsql:squery(Conn, Query),
    #{up_by:=UpBy} = util:get_mapped_records(Columns, Res),

    case PlayerID == UpBy of
        true -> 
            QuerySet = util:set_query(<<"circle_up">>, #{cancelled=>true}, #{circle_id=>CircleID, circle_up_id=>CircleUpID}),
            epgsql:squery(Conn, QuerySet);
        _ ->
            #{error=>"cancelling not upped by you"}
    end.
