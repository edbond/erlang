-module(tproxy).
-author("<edbond@gmail.com>").
-vsn(1.0).

-behaviour(gen_server).

-define(PORT, 3456).
-define(MAX_CONNECTS, 3000).

-compile(export_all).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% HiPE
-mode(compile).
-compile( [ native, { hipe, o3 } ] ).
-compile( [ inline, { inline_size, 100 } ] ).


% server state ?
-record(tp_state,
  {
    port, % port connection accepted on
    listen, % socket I'm listen on
    socket % socket I talk to
  }
).

% gen_server
init(Args) ->
  io:format("init: ~p~n", [Args]),
  % bind
  {ok, Sock} = gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, false}]),
  io:format("listen on (~p): ~p~n", [?PORT, Sock]),
  % listen
  {ok, #tp_state{listen=Sock}}.


handle_call(Request, From, State) ->
  io:format("handle_call: ~p~n", [Request]),
  {reply, From, State}.

handle_cast({accept}, State) ->
  case gen_tcp:accept(State#tp_state.listen, infinity) of
    {ok, Socket} -> 
      io:format("going into loop ~p~n", [Socket]),
      Controller = spawn(fun() -> controller() end),
      gen_tcp:controlling_process(Socket, Controller),
      gen_server:cast(self(), {loop, Socket, Controller});
      %{noreply, State#tp_state{socket=Socket} };
    {error, timeout} ->
      io:format("restart accept~n"),
      %% restart
      gen_server:cast(self(), {accept})
  end,
  gen_server:cast(self(), {accept}),
  {noreply, State};
handle_cast({loop, Socket, Controller}, State) ->
  spawn(fun() -> loop(Socket, Controller) end),
  {noreply, State};
handle_cast(Request, State) ->
  io:format("handle_cast: ~p~n", [Request]),
  {noreply, State}.

handle_info(Info, State) ->
  io:format("handle_info: ~p~n", [Info]),
  {reply, Info, State}.

terminate(Reason, _State) ->
  io:format("terminate: ~p~n", [Reason]),
  ok.

code_change(OldVsn, _State, _Extra) ->
  io:format("code_change: ~p~n", [OldVsn]),
  updated.


-record(request,
  {
    method, % GET or POST or ...
    url, % url
    version, % HTTP/1.0 or HTTP/1.1
    headers % array of headers
  }
).

%get_response(Request) ->
  %{ok, {{Version, 200, ReasonPhrase}, Headers, Body}} = http:request(Request#request.url),
  %Body.

parse_headers(Request, R) ->
  io:format("request: ~p result: ~p~n", [Request, R]),
  Lines = string:tokens(Request, "\r\n"),
  io:format("Lines: ~p~n", [Lines]),
  Headers = lists:nthtail(1, Lines),
  FirstLine = hd(Lines),
  io:format("First: ~p~nHeaders: ~p~n", [FirstLine, Headers]),

  [Method, URL, Version] = string:tokens(FirstLine, " "),
  io:format("Method, URL, Version = ~p, ~p, ~p~n", [Method, URL, Version]),

  R#request{method = Method, url = URL, version = Version, headers = Headers}.

parse_headers(Request) ->
  parse_headers(Request, #request{}).

parse_request(Data, _Pid) ->
  % <<"GET http://www.google.com/ HTTP/1.1\r\nAccept: */*\r\nHost: www.google.com\r\n\r\n">>
  Request = binary_to_list(Data),
  Req = parse_headers(Request),
  io:format("request: ~p, ~p~n", [Req#request.method, Req#request.url]),
  Req.

controller() ->
  receive
    Message ->
      io:format("got message: ~p~n", [Message])
  end,
  controller().

pair(Read, Write, Data) when Data /= void ->
  gen_tcp:send(Write, Data),
  pair(Read, Write, void);

pair(Read, Write, void) ->
  % main read/write loop
  case gen_tcp:recv(Read, 0) of
    {ok, B} ->
      io:format("Got data from ~p: ~p. Send this to ~p", [Read, B, Write]),
      gen_tcp:send(Write, B),
      pair(Read, Write, void);
    {error, closed} ->
      io:format("closed ~p <=> ~p~n", [Read, Write]),
      ok
  end.

make_pair(Request, Client, InitialData, Controller) ->
  Uri = uri:from_string(Request#request.url),
  Address = uri:host(Uri),
  case uri:port(Uri) of
    [] ->
      Port = 80;
    Other ->
      Port = Other
  end,

  io:format("connecting to ~p:~p~n", [Address,Port]),

  {ok, Server} = gen_tcp:connect(Address, Port, []),
  io:format("a~n",[]),
  ok = gen_tcp:controlling_process(Server, Controller),
  io:format("b~n",[]),
  gen_tcp:send(Server, InitialData).

  % spawn process that will recv from Client and send to Server
  %spawn(fun() -> pair(Client, Server, InitialData) end),
  % spawn process that will recv from Server and send to Client
  %spawn(fun() -> pair(Server, Client, void) end).

%% client <-> me loop
loop(Socket, Controller) ->
  io:format("loop ~n",[]),
  case gen_tcp:recv(Socket,0) of
    {ok, Data} ->
      io:format("read data ~p~n", [Data]),
      % parse data
      Request = parse_request( Data, self() ),
      make_pair(Request, Socket, Data, Controller);
    {error, Reason} ->
      io:format("socket closed ~p~n", [Reason]);
    Other ->
      io:format("unknown recv result ~p~n", [Other])
  end.

main() ->
  {ok, Pid} = gen_server:start_link(?MODULE, [], []),
  io:format("started gen_server pid: ~p~n", [Pid]),
  inets:start(),
  gen_server:cast(Pid, {accept}).

  %io:format("yo! ~p~n", [Body]),
  %inets:stop().
