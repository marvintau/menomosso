-module(mail_reply).

-export([get/2, add/2]).

get(Conn, MailID) ->
    Query = util:get_query(<<"mail_reply">>, #{mail_id=>MailID}),
    {ok, Column, Data} = epgsql:squery(Conn, Query),
    Res = util:get_mapped_records(Column, Data),

    SortedRes = lists:sort(fun(#{reply_seq:=RepSeqA}, #{reply_seq:=RepSeqB}) -> RepSeqA < RepSeqB end, Res),

    SortedRes.

add(Conn, Insertion) ->
    
    quickrand:seed(),
    ReplyID = list_to_binary(uuid:uuid_to_string(uuid:get_v4_urandom())),

    erlang:display(ReplyID),

    Query = util:add_query(<<"mail_reply">>, Insertion#{reply_id=>ReplyID}),
    {ok, _} = epgsql:squery(Conn, Query),
    
    ReplyID.
