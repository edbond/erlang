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

% convert xml from Encoding to UTF-8
decode(Encoding, Xml) ->
  %% substitute <?xml ?> 
  Xml.

parse_fb2(Filename, Acc, Encoding) ->
  io:format("parse ~p~n", [Encoding]),
  {ok, Xml}=file:read_file(Filename),
  if
    Encoding == "utf-8" ->
      Utf8Xml = Xml;
    true ->
      Utf8Xml = decode(Encoding, Xml)
  end,

  {ok, ParserFSM}=gen_fsm:start_link(fb2_fsm, [], []),
  try erlsom:parse_sax(Utf8Xml, [], fun(F,A) -> sax_event(ParserFSM, F,A) end)
  catch {error, Error} ->
    %% Encoding windows-1251 not supported
    case re:run(Error, "Encoding (.*) not supported") of
      nomatch ->
        io:format("error: ~p~n", [Error]);
      {match, Captured} ->
        %% transcode
        {Start, Length} = lists:last(Captured),
        NewEncoding = string:substr(Error, Start+1, Length),
        parse_fb2(Filename, Acc, NewEncoding)
    end
  end,
  [Filename]++Acc.

start() ->
  Results=filelib:fold_files(".", ".*\.fb2$", false, fun(F,A) -> parse_fb2(F, A, "utf-8") end, []),
  io:format("results ~p~n", [Results]),
  ok.
