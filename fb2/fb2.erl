-module(fb2).
-compile(export_all).

-record(fb2_info, {authors=[], title="", annotation="", encoding="", filename=""}).

% convert xml from Encoding to UTF-8
decode(Encoding, Xml) ->
  case Encoding of
    "windows-1251" ->
      %io:format("decode: ~s~n", [Encoding]),
      win1251:decode(Xml);
    "utf-8" ->
      Xml;
    "UTF-8" ->
      Xml
  end.

extract(Match, String) ->
  {Start, Length}=lists:nth(2,Match),
  string:substr(String, Start+1, Length).

cut_re(String, Re, Options, Default) ->
  case re:run(String, Re, Options) of
    {match, Match} ->
      extract(Match, String);
    nomatch ->
      Default
  end.

parse_authors(Xml, Matches, Encoding) ->
  M=lists:map(fun(X) -> extract(X, Xml) end, Matches),
  %io:format("M ~p~n", [M]),

  Authors=lists:foldl(fun(A, Acc) ->
        FirstName=cut_re( A, "<first-name>(.*?)</first-name>", [dotall], ""),
        LastName=cut_re( A, "<last-name>(.*?)</last-name>", [dotall], ""),
        Acc ++ [{decode(Encoding,FirstName), decode(Encoding,LastName)}]
    end, [], M),

  %io:format("authors: ~p~n", [Authors]),
  {ok, Authors}.

parse_fb2(Filename, Acc) ->
  %io:format("parse ~p~n", [Filename]),
  {ok, XmlFile}=file:open(Filename, [read]),
  {ok, Xml}=file:read(XmlFile, 2048),
  ok=file:close(XmlFile),

  %% determine encoding
  Encoding=cut_re(Xml, "<\?.*encoding=\"(.+?)\".*\?>", [], "utf-8"),
  %io:format("encoding = ~p~n", [Encoding]),

  TitleInfo=cut_re(Xml, "<title-info>(.*?)</title-info>", [dotall], ""),
  %io:format("title info: ~p~n", [TitleInfo]),

  %% find <author></author>
  case re:run(TitleInfo, "<author>(.*?)</author>", [ global | [dotall] ]) of
    {match, Match} ->
      {ok, Authors}=parse_authors(TitleInfo, Match, Encoding);
    nomatch ->
      Authors=[]
  end,
  Title=decode(Encoding, cut_re(TitleInfo, "<book-title>(.*?)</book-title>", [dotall], "")),
  Annotation=decode(Encoding, cut_re(TitleInfo, "<annotation>(.*?)</annotation>", [dotall], "")),

  [#fb2_info{encoding=Encoding, filename=Filename, authors=Authors, title=Title, annotation=Annotation}]++Acc.

dump_info(Book) ->
  io:format("~n~nfilename: ~s~n", [Book#fb2_info.filename]),
  io:format("title: ~s~n", [Book#fb2_info.title]),
  io:format("annotation: ~s~n", [Book#fb2_info.annotation]),

  lists:map(fun(A) ->
        io:format("author: ~s ~s~n", [element(1,A), element(2,A)]) end,
        Book#fb2_info.authors
      ),
  ok.

start() ->
  Results=filelib:fold_files(".", ".*\.fb2$", true, fun(F,A) -> parse_fb2(F, A) end, []),
  lists:map(fun dump_info/1, Results),
  %io:format("results ~p~n", [Results]),
  ok.
