-module(circle).

-export([get/3, add/3, delete/3]).

get(Conn, PlayerID, Offset) ->

    Query =list_to_binary(["select * from circle where receiver_id='", PlayerID,"' order by last_reply_time limit 20 offset ", Offset, ";"]),
    {ok, Columns, Res} = epgsql:squery(Conn, Query),
    Result = util:get_mapped_records(Columns, Res),

    Result.

select(PlayerID, PlayerID, ID) -> ID;
select(PlayerID, ID, PlayerID) -> ID.

add(Conn, PlayerID, Content) ->

    FriendList = friend:get(Conn, PlayerID),
    FriendIDList = [select(PlayerID, IDA, IDB) || #{friend_a:=IDA, friend_b:=IDB} <- FriendList],

    CircleList = [#{sender_id=>PlayerID, receiver_id=>FriendID, content=>Content} || FriendID <- FriendIDList],

    Query = util:add_query(<<"circle">>, CircleList),
    {ok, 1} = epgsql:squery(Conn, Query),

    #{ok => added}.

delete(Conn, CircleID, PlayerID) ->

    Query = util:get_query(<<"circle_up">>, #{circle_id=>CircleID}),
    {ok, Columns, Res} = epgsql:squery(Conn, Query),
    #{post_by:=PostBy} = util:get_mapped_records(Columns, Res),

    case PlayerID == PostBy of
        true -> 
            QuerySet = util:set_query(<<"circle_up">>, #{is_deleted=>true}, #{circle_id=>CircleID}),
            epgsql:squery(Conn, QuerySet),

            #{ok=>cancelled};
        _ ->
            #{error=>"cancelling not upped by you"}
    end.
