-module(card_types).

-export([get/2]).

get(Conn, CardID) ->
    
    Query = util:get_query(<<"card_types">>, #{card_id=>CardID}),
    {ok, ColumnSpec, Result} = epgsql:squery(Conn, Query),
    [Res] = util:get_mapped_records(ColumnSpec, Result),
    {ok, Res}.
