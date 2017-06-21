-module(card_skills).

-export([get/1]).

get(Conn) ->
    Query = util:get_query(<<"card_skills">>),

    {ok, ColumnSpec, Res} = epgsql:squery(Conn, Query),
    Result = util:get_mapped_records(ColumnSpec, Res),
    {ok, Result}.
