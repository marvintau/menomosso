-module(mail).

-export([get/2, get/3, set/4, add/2]).

get(Conn, ReceiverID) ->
     Query = util:get_query(<<"mail">>, #{receiver_id=>ReceiverID}),
     {ok, Columns, Mails} = epgsql:squery(Conn, Query),

     Res = util:get_mapped_records(Columns, Mails),

     Res.

get(Conn, ReceiverID, MailID) ->
    Query = util:get_query(<<"mail">>, #{receiver_id=>ReceiverID, mail_id=>MailID}),
    {ok, Columns, Mails} = epgsql:squery(Conn, Query),
    [Res] = util:get_mapped_records(Columns, Mails),
    Res.

set(Conn, Setting, ReceiverID, MailID) ->
    Query = util:set_query(<<"mail">>, Setting, #{receiver_id=>ReceiverID, mail_id=>MailID}),
    {ok, _} = epgsql:squery(Conn, Query).

add(Conn, Insertion) ->

    quickrand:seed(),
    MailID = list_to_binary(uuid:uuid_to_string(uuid:get_v4_urandom())),

    Query = util:add_query(<<"mail">>, Insertion#{mail_id=>MailID}),
    {ok, _} = epgsql:squery(Conn, Query),

    MailID.

