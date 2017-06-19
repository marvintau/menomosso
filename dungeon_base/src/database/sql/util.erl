-module(util).

-export([array_to_list/1, get_mapped_records/2, get_mapped_records_context/2]).

array_to_list(Array) ->
    Trimmed = list_to_binary(tl(lists:droplast(binary_to_list(Array)))),
    binary:split(Trimmed, <<",">>, [global]).

range_to_list(Range) ->
    Trimmed = list_to_binary(tl(lists:droplast(binary_to_list(Range)))),
    [A, B] = binary:split(Trimmed, <<",">>, [global]),
    [binary_to_integer(A), binary_to_integer(B)].

type_convert_helper(int4, Field)        -> binary_to_integer(Field);
type_convert_helper(int4range, Field)   -> range_to_list(Field);
type_convert_helper({array, _}, Field)  -> array_to_list(Field);
type_convert_helper(_, Field)           -> Field.

type_convert_helper_context(int4, Field)        -> {single, binary_to_integer(Field)};
type_convert_helper_context(int4range, Field)   -> {range, range_to_list(Field)};
type_convert_helper_context({array, _}, Field)  -> {single, array_to_list(Field)};
type_convert_helper_context(_, Field)           -> {single, Field}.

type_convert(ZippedRecord) ->
    [ {Name, type_convert_helper(Type, Field)} || {{Name, Type}, Field} <- ZippedRecord].
type_convert_context(ZippedRecord) ->
    [ {Name, type_convert_helper_context(Type, Field)} || {{Name, Type}, Field} <- ZippedRecord].


% 适用于select查询语句
get_mapped_records(ColumnSpec, Records) ->
    ColumnNames = [ {binary_to_atom(Name, utf8), Type} || {_, Name, Type, _, _, _} <- ColumnSpec],
    ListedRecords = [tuple_to_list(Record) || Record <-Records],
    [maps:from_list(type_convert(lists:zip(ColumnNames, ListedRecord))) || ListedRecord <- ListedRecords].

get_mapped_records_context(ColumnSpec, Records) ->
    ColumnNames = [ {binary_to_atom(Name, utf8), Type} || {_, Name, Type, _, _, _} <- ColumnSpec],
    ListedRecords = [tuple_to_list(Record) || Record <-Records],
    [maps:from_list(type_convert_context(lists:zip(ColumnNames, ListedRecord))) || ListedRecord <- ListedRecords].
