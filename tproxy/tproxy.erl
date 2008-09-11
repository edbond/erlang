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
      gen_server:cast(self(), {loop, Socket});
      %{noreply, State#tp_state{socket=Socket} };
    {error, timeout} ->
      io:format("restart accept~n"),
      %% restart
      gen_server:cast(self(), {accept})
  end,
  gen_server:cast(self(), {accept}),
  {noreply, State};
handle_cast({loop, Socket}, State) ->
  spawn(fun() -> loop(Socket) end),
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
    method, % port connection accepted on
    url, % socket I'm listen on
    headers % socket I talk to
  }
).

parse_headers(Request, R) ->
  io:format("request: ~p result: ~p~n", [Request, R]),
  [Line, Rest] = lists:split(fun() -> "", Request
  R.

parse_headers(Request) ->
  parse_headers(Request, #request{}).

parse_request(Data, _Pid) ->
  % <<"GET http://www.google.com/ HTTP/1.1\r\nAccept: */*\r\nHost: www.google.com\r\n\r\n">>
  Request = binary_to_list(Data),
  Req = parse_headers(Request),
  io:format("request: ~p, ~p~n", [Req#request.method, Req#request.url]),
  ok.
  
%% client <-> me loop
loop(Socket) ->
  io:format("loop ~n",[]),
  case gen_tcp:recv(Socket,0) of
    {ok, Data} ->
      io:format("read data ~p~n", [Data]),
      % parse data
      parse_request( Data, self() ),
      gen_tcp:send(Socket, Data),
      loop(Socket);
    {error, Reason} ->
      io:format("socket closed ~p~n", [Reason]);
    Other ->
      io:format("unknown recv result ~p~n", [Other])
  end.

main() ->
  {ok, Pid} = gen_server:start_link(?MODULE, [], []),
  io:format("started gen_server pid: ~p~n", [Pid]),
  gen_server:cast(Pid, {accept}).

  %inets:start(),
  %{ok, {{Version, 200, ReasonPhrase}, Headers, Body}} = http:request("http://www.erlang.org"),
  %io:format("yo! ~p~n", [Body]),
  %inets:stop().
