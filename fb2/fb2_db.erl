-module(fb2_db).
-compile(export_all).

-record(fb2_info, {authors=[], title="", annotation="", encoding="", filename=""}).

start() ->
  mnesia:create_schema([node()]),
  mnesia:start().

cleanup() ->
  try
    mnesia:table_info(fb2_info, type)
    %{atomic, ok}=mnesia:clear_table(fb2_info)
  catch
    exit: _ ->
      io:format("create table~n"),
      mnesia:create_table(fb2_info,
        [{attributes, record_info(fields, fb2_info)},
        {type, set},
        {disc_copies, [node()]}])
  end,
  ok.

insert(Books) ->
  mnesia:transaction(fun() ->
        [mnesia:write(B) || B <- Books]
    end
  ),
  ok.

get_all() ->
  {atomic, Results}=mnesia:transaction(fun() ->
        mnesia:foldl(fun(Book, Acc) -> Acc ++ [Book] end, [], fb2_info)
  end),
  Results.
