-module(mail_item).

-export([add/4]).

add(Conn, MailID, ItemID, ItemQty) ->
    Query = util:add_query(<<"mail_item">>, #{mail_id=>MailID, item_id=>ItemID, item_qty=>ItemQty}),
    {ok, _} = epgsql:squery(Conn, Query).


