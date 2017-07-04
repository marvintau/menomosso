-module(circle).

-export([get/2, add/3, delete/3]).

get(Conn, PlayerID) ->
    Query1 = util:get_query(<<"circle">>, #{player_id=>PlayerID}),
    {ok, Columns, Res1} = epgsql:squery(Conn, Query1),
    Result1 = util:get_mapped_records(Columns, Res1),

    Result1.

add(Conn, PlayerID, Content) ->
    Query = util:add_query(<<"circle">>, #{player_id=>PlayerID, content=>Content}),
    {ok, 1} = epgsql:squery(Conn, Query),

    {ok, added}.

delete(Conn, CircleID, PlayerID) ->

    Query = util:get_query(<<"circle_up">>, #{circle_id=>CircleID}),
    {ok, Columns, Res} = epgsql:squery(Conn, Query),
    #{post_by:=PostBy} = util:get_mapped_records(Columns, Res),

    case PlayerID == PostBy of
        true -> 
            QuerySet = util:set_query(<<"circle_up">>, #{is_deleted=>true}, #{circle_id=>CircleID}),
            epgsql:squery(Conn, QuerySet);
        _ ->
            #{error=>"cancelling not upped by you"}
    end.
