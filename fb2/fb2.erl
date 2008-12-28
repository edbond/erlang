-module(fb2).
-compile(export_all).

-record(fb2_info, {authors=[], title="", annotation=""}).

% convert xml from Encoding to UTF-8
decode(Encoding, Xml) ->
  case Encoding of
    "windows-1251" ->
      io:format("decode: ~s~n", [Encoding]),
      win1251:decode(Xml);
    "utf-8" ->
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
  io:format("zz ~w~n", [decode(Encoding, lists:flatten(M) )]).

parse_fb2(Filename, Acc) ->
  io:format("parse ~p~n", [Filename]),
  {ok, XmlFile}=file:open(Filename, [read]),
  {ok, Xml}=file:read(XmlFile, 2048),
  ok=file:close(XmlFile),

  Fb2Empty=#fb2_info{},

  %% determine encoding
  Encoding=cut_re(Xml, "<\?.*encoding=\"(.+)\".*/?>", [], "utf-8"),
  io:format("encoding = ~p~n", [Encoding]),

  TitleInfo=cut_re(Xml, "<title-info>(.+?)</title-info>", [dotall], ""),
  %io:format("title info: ~p~n", [TitleInfo]),

  %% find <author></author>
  case re:run(TitleInfo, "<author>(.+?)</author>", [global | [dotall]]) of
    {match, Authors} ->
      AuthorsList=parse_authors(TitleInfo, Authors, Encoding);
    nomatch ->
      AuthorsList=[]
  end,

  [Filename]++Acc.

start() ->
  Results=filelib:fold_files(".", ".*\.fb2$", false, fun(F,A) -> parse_fb2(F, A) end, []),
  io:format("results ~p~n", [Results]),
  ok.
