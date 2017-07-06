-module(friend_request).

-export([add/3, get/2, accept/3, reject/3]).

add(Conn, SenderID, ReceiverID) ->
    
    Query = util:add_query(<<"friend_request">>, #{sender_id=>SenderID, receiver_id=>ReceiverID}),

    {ok, 1} = epgsql:squery(Conn, Query),

    #{ok => added}.

get(Conn, ReceiverID) ->

    Query = util:get_query(<<"friend_request">>, #{receiver_id=>ReceiverID}),

    {ok, Column, Res} = epgsql:squery(Conn, Query),

    util:get_mapped_records(Column, Res).

accept(Conn, RequestID, ReceiverID) ->

    Query = util:get_query(<<"friend_request">>, #{request_id=>RequestID}),

    {ok, Column, Res} = epgsql:squery(Conn, Query),
    [#{receiver_id:=RequestedReceiver, sender_id:=SenderID}] = util:get_mapped_records(Column, Res),

    case RequestedReceiver of
        ReceiverID ->

            QuerySet = util:set_query(<<"friend_request">>, #{is_accepted=>true}, #{request_id=>RequestID}),
            {ok, 1} = epgsql:squery(Conn, QuerySet),

            friend:add(Conn, ReceiverID, SenderID),

            #{ok=>"accepted"};

        _ ->
            #{error=>"you are not authorized to answer this request"}
    end.

reject(Conn, RequestID, ReceiverID) ->

    Query = util:get_query(<<"friend_request">>, #{request_id=>RequestID}),

    {ok, Column, Res} = epgsql:squery(Conn, Query),
    [#{receiver_id:=RequestedReceiver}] = util:get_mapped_records(Column, Res),

    case RequestedReceiver of
        ReceiverID ->

            QuerySet = util:set_query(<<"friend_request">>, #{is_rejected=>true}, #{request_id=>RequestID}),
            {ok, 1} = epgsql:squery(Conn, QuerySet),
            #{ok=>"rejected"};

        _ ->
            #{error=>"you are not authorized to answer this request"}
    end.
