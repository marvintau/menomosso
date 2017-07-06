-module(friend_request).

-export([]).

add(Conn, SenderID, ReceiverID) ->
    
    Query = util:add_query(<<"friend_request">>, #{sender_id=>SenderID, receiver_id=>ReceiverID}),

    {ok, 1} = epgsql:squery(Conn, Query),

    {ok, added}.

get(Conn, ReceiverID) ->

    Query = util:get_query(<<"friend_request">>, #{receiver_id=>ReceiverID}),

    {ok, Column, Res} = epgsql:squery(Conn, Query),

    Result = util:get_mapped_records(Column, Res).

accept(Conn, RequestID, ReceiverID) ->

    Query = util:get_query(<<"friend_request">>, #{request_id=>RequestID}),

    {ok, Column, Res} = epgsql:squery(Conn, Query),
    [#{receiver_id:=RequestedReceiver}=Result] = util:get_mapped_records(Column, Res),

    case RequestedReceiver of
        ReceiverID ->

            QuerySet = util:set_query(<<"friend_request">>, #{is_accepted=>true}, #{request_id=>RequestID}),
            {ok, 1} = epgsql:squery(Conn, QuerySet),
            #{ok=>"added"};

        _ ->
            #{error=>"you are not authorized to answer this request"}
    end.
