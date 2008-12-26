-module(fb2).
-compile(export_all).
-import(erlsom, [parse_sax/3]).

sax_event(ParserFSM, Event, A) ->
  %io:format("~p~n", [Event]),
  case Event of
    startDocument -> ok;
    endDocument -> ok;
    {processingInstruction, _Target, _Data} -> ok;
    {startPrefixMapping, _Prefix , _URI} -> ok;
    {endPrefixMapping, _Prefix} -> ok;
    {ignorableWhitespace, _WS} -> ok;
    {startElement, _Uri, LocalName, _Prefix, _Attributes} ->
      gen_fsm:sync_send_event(ParserFSM, {element, LocalName});
    {characters, Chars} ->
      gen_fsm:sync_send_event(ParserFSM, {characters, Chars});
    {endElement, _Uri, LocalName, _Prefix} ->
      gen_fsm:sync_send_event(ParserFSM, {end_element, LocalName});
    Other ->
      io:format("unknown event: ~p~n", [Other])
  end,
  A.

parse_fb2(Filename, Acc) ->
  {ok, Xml}=file:read_file(Filename),
  {ok, ParserFSM}=gen_fsm:start_link(fb2_fsm, [], []),
  try erlsom:parse_sax(Xml, [], fun(F,A) -> sax_event(ParserFSM, F,A) end)
  catch {error, Error} ->
    io:format("error: ~p~n", [Error])
  end,
  [Filename]++Acc.

start() ->
  Results=filelib:fold_files(".", ".*\.fb2$", false, fun(F,A) -> parse_fb2(F, A) end, []),
  io:format("results ~p~n", [Results]),
  ok.
