-module(mail_item).

-export([add/4, get/2]).

add(Conn, MailID, ItemID, ItemQty) ->
    Query = util:add_query(<<"mail_item">>, #{mail_id=>MailID, item_id=>ItemID, item_qty=>ItemQty}),
    {ok, _} = epgsql:squery(Conn, Query).

get(Conn, MailID) ->
    Query = util:get_query(<<"mail_item">>, #{mail_id=>MailID}),
    {ok, Columns, Result} = epgsql:squery(Conn, Query),
    Res = util:get_mapped_records(Columns, Result),
    Res.
