-module(card_type).

-export([get/2, get_context/2]).

get(Conn, CardID) ->
    
    Query = util:get_query(<<"card_type">>, #{card_id=>CardID}),
    {ok, ColumnSpec, Result} = epgsql:squery(Conn, Query),
    [Res] = util:get_mapped_records(ColumnSpec, Result),
    Res.

get_context(Conn, CardID) ->
    
    Query = util:get_query(<<"card_type">>, #{card_id=>CardID}),
    {ok, ColumnSpec, Result} = epgsql:squery(Conn, Query),
    [Res] = util:get_mapped_records_context(ColumnSpec, Result),
    Res.
