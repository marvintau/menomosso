-module(util).

-export([array_to_list/1, get_mapped_records/2, get_mapped_records_context/2, add_query/2, set_query/3]).
-compile([export_all]).


erl2psql_typeconv({e, Exp})                                    -> Exp;
erl2psql_typeconv(Int) when is_integer(Int)                    -> integer_to_binary(Int);
erl2psql_typeconv([A, B]) when is_integer(A) and is_integer(B) -> list_to_binary(["'[", integer_to_binary(A), ",", integer_to_binary(B), ")'"]);
erl2psql_typeconv(List) when is_list(List)                     -> list_to_array(List);
erl2psql_typeconv(Atom) when is_atom(Atom)                     -> list_to_binary([atom_to_binary(Atom, utf8)]);
erl2psql_typeconv(Binary) when is_binary(Binary)               -> list_to_binary(["'", Binary, "'"]).

psql2erl_typeconv(int4, Field)        -> binary_to_integer(Field);
psql2erl_typeconv(int4range, Field)   -> range_to_list(Field);
psql2erl_typeconv({array, _}, Field)  -> array_to_list(Field);
psql2erl_typeconv(_, Field)           -> Field.

psql2erl_typeconv_context(int4, Field)        -> {single, binary_to_integer(Field)};
psql2erl_typeconv_context(int4range, Field)   -> {range, range_to_list(Field)};
psql2erl_typeconv_context({array, _}, Field)  -> {single, array_to_list(Field)};
psql2erl_typeconv_context(_, Field)           -> {single, Field}.


array_to_list(Array) ->
    Trimmed = list_to_binary(tl(lists:droplast(binary_to_list(Array)))),
    binary:split(Trimmed, <<",">>, [global]).



list_to_delimited(List, Del) when is_list(List) ->
    list_to_delimited(List, [], Del).
list_to_delimited([Item], Res, _) ->
    lists:reverse([Item | Res]);
list_to_delimited([Item | Rem], Res, Del) ->
    list_to_delimited(Rem, [list_to_binary([Item, Del]) | Res], Del).


list_to_array(Array) ->
    list_to_binary(["{", Array, "}"]).

range_to_list(Range) ->
    Trimmed = list_to_binary(tl(lists:droplast(binary_to_list(Range)))),
    [A, B] = binary:split(Trimmed, <<",">>, [global]),
    [binary_to_integer(A), binary_to_integer(B)].




psql2erl(ZippedRecord) ->
    [ {Name, psql2erl_typeconv(Type, Field)} || {{Name, Type}, Field} <- ZippedRecord].
psql2erl_context(ZippedRecord) ->
    [ {Name, psql2erl_typeconv_context(Type, Field)} || {{Name, Type}, Field} <- ZippedRecord].


get_assignments(PropList) ->
    lists:map(fun({K, V}) -> list_to_binary([atom_to_binary(K, utf8), "=", util:erl2psql_typeconv(V)]) end, PropList).



% 适用于select查询语句
get_mapped_records(ColumnSpec, Records) ->
    ColumnNames   = [ {binary_to_atom(Name, utf8), Type} || {_, Name, Type, _, _, _} <- ColumnSpec],
    ListedRecords = [tuple_to_list(Record) || Record <-Records],
    [maps:from_list(psql2erl(lists:zip(ColumnNames, ListedRecord))) || ListedRecord <- ListedRecords].

get_mapped_records_context(ColumnSpec, Records) ->
    ColumnNames   = [ {binary_to_atom(Name, utf8), Type} || {_, Name, Type, _, _, _} <- ColumnSpec],
    ListedRecords = [tuple_to_list(Record) || Record <-Records],
    [maps:from_list(psql2erl_context(lists:zip(ColumnNames, ListedRecord))) || ListedRecord <- ListedRecords].


% 适用于update
set_query(TableName, AssignMap, CondMap) ->
    
    Assignments = list_to_delimited(get_assignments(maps:to_list(AssignMap)), ", "),
    Conds       = list_to_delimited(get_assignments(maps:to_list(CondMap)), " and "),

    list_to_binary(["update ", TableName, " set ", Assignments, " where ", Conds, ";"]).

% insert
add_query(TableName, ValueMapTable) when is_list(ValueMapTable) ->

    KVList         = [lists:unzip(maps:to_list(ValueMap)) || ValueMap <- ValueMapTable],
    {Keys,_}       = lists:nth(1, KVList),
    KeysString     = list_to_binary(["(", list_to_delimited(lists:map(fun erl2psql_typeconv/1, Keys), ", "), ")"]),
    
    ValueTable     = [element(2, KV) || KV <- KVList],
    ValuesStrings  = [list_to_binary(["(", list_to_delimited(lists:map(fun erl2psql_typeconv/1, Values), ", "), ")"]) || Values <- ValueTable],
    ValuesString   = list_to_delimited(ValuesStrings, ",\n"),

    list_to_binary(["insert into ", TableName, KeysString, " values", ValuesString, ";"]);

add_query(TableName, ValueMap) ->

    {Keys, Values} = lists:unzip(maps:to_list(ValueMap)),
    KeysString     = list_to_binary(["(", list_to_delimited(lists:map(fun erl2psql_typeconv/1, Keys), ", "), ")"]),
    ValuesString   = list_to_binary(["(", list_to_delimited(lists:map(fun erl2psql_typeconv/1, Values), ", "), ")"]),

    list_to_binary(["insert into ", TableName, KeysString, " values", ValuesString, ";"]).

get_query(TableName) ->
    list_to_binary(["select * from ", TableName, ";" ]).
get_query(TableName, CondMap) ->
    Conds       = list_to_delimited(get_assignments(maps:to_list(CondMap)), " and "),
    list_to_binary(["select * from ", TableName, " where ", Conds, ";" ]).
get_query(TableName, CondMap, OrderBy) ->
    Conds       = list_to_delimited(get_assignments(maps:to_list(CondMap)), " and "),
    list_to_binary(["select * from ", TableName, " where ", Conds, " order by ", OrderBy, ";" ]).
