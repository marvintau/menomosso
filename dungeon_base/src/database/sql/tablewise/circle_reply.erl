-module(circle_reply).

-export([get/2, add/3, delete/4]).

get(Conn, CircleID) ->
    Query1 = util:get_query(<<"circle_reply">>, #{circle_id=>CircleID}),
    {ok, Columns, Res1} = epgsql:squery(Conn, Query1),
    Result1 = util:get_mapped_records(Columns, Res1),

    Result1.

add(Conn, PlayerID, Content) ->
    Query = util:add_query(<<"circle_reply">>, #{player_id=>PlayerID, content=>Content}),
    {ok, 1} = epgsql:squery(Conn, Query),

    {ok, added}.

delete(Conn, CircleID, CircleReplyID, PlayerID) ->

    Query = util:get_query(<<"circle_up">>, #{circle_id=>CircleID, circle_reply_id=>CircleReplyID}),
    {ok, Columns, Res} = epgsql:squery(Conn, Query),
    #{reply_by:=ReplyBy} = util:get_mapped_records(Columns, Res),

    case PlayerID == ReplyBy of
        true -> 
            QuerySet = util:set_query(<<"circle_up">>, #{is_deleted=>true}, #{circle_id=>CircleID, circle_reply_id=>CircleReplyID}),
            epgsql:squery(Conn, QuerySet);
        _ ->
            #{error=>"cancelling not upped by you"}
    end.
