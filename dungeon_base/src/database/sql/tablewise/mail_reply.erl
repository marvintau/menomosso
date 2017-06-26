-module(mail_reply).

-export([get/2, add/2]).

get(Conn, MailID) ->
    Query = util:get_query(<<"mail_reply">>, #{mail_id=>MailID}),
    {ok, Column, Data} = epgsql:squery(Conn, Query),
    Res = util:get_mapped_records(Column, Data),

    SortedRes = lists:sort(fun(#{reply_seq:=RepSeqA}, #{reply_seq:=RepSeqB}) -> RepSeqA < RepSeqB end, Res),

    SortedRes.

add(Conn, Insertion) ->
    Query = util:add_query(<<"mail_reply">>, #{mail_id=>Insertion}),
    {ok, _} = epgsql:squery(Conn, Query).
