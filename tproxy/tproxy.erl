-module(tproxy).
-author("<edbond@gmail.com>").
-vsn(1.0).

%-behaviour(gen_server).

-define(PORT, 3456).
-define(MAX_CONNECTS, 3000).
-define(TCP_OPTS, [list, {packet, raw}, {nodelay, true}, {reuseaddr, true}, {active, false}]).

-define(DEBUG, false).

-compile(export_all).
%-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% HiPE
%-mode(compile).
%-compile( [ native, { hipe, o3 } ] ).
%-compile( [ inline, { inline_size, 100 } ] ).

log(A, B) ->
  case ?DEBUG of
    true ->
      io:format(A, B);
    _Other ->
      ok
  end.

read_until(Stop, Socket, Acc) ->
  log("read_until ~p ~p ~p~n", [Stop, Socket, Acc]),
  {ok, Data} = gen_tcp:recv(Socket, 0),
  log("read ~p~n", [Data]),
  NewAcc = [Acc | Data],
  Pos = string:str(Data, Stop),
  if
    Pos == 0 ->
      %log("read more...~n", []),
      read_until(Stop, Socket, NewAcc);
    true ->
      %log("all~n", []),
      ok
  end,
  log("returning ~p~n", [NewAcc]),
  lists:flatten(NewAcc).

conversation(ClientSocket, SocketsTable) ->
  Request = read_until("\r\n\r\n", ClientSocket, []),
  % parse request
  Uri = uri:from_string(Request),
  Address = uri:host(Uri),
  case uri:port(Uri) of
    [] ->
      Port = 80;
    Other ->
      Port = Other
  end,
  
  % connect to server
  log("connecting to ~p:~p~n", [Address,Port]),

  {ok, ServerSocket} = gen_tcp:connect(Address, Port, []),
  log("socket to server ~p ~n",[ServerSocket]),

  gen_tcp:send(ServerSocket, Request),
  % store connection info in table
  NewTable=lists:append(SocketsTable, [{ClientSocket, ServerSocket}, {ServerSocket, ClientSocket}]),
  conversation(ClientSocket, ServerSocket, NewTable).


conversation(ClientSocket, ServerSocket, SocketsTable) ->
  receive
    {tcp, From, Packets} ->
      log("got message: ~p ~p~n", [From, Packets]),
      % find endpoint from list
      {value, Pair} = lists:keysearch(From, 1, SocketsTable),
      Destination = element(2, Pair),
      ok = gen_tcp:send(Destination, Packets),
      conversation(ClientSocket, ServerSocket, SocketsTable);
    {tcp_closed, Port} ->
      log("closed message ~p ~n", [Port]),
      {value, Pair} = lists:keysearch(Port, 1, SocketsTable),
      ok = gen_tcp:close(element(1, Pair)),
      ok = gen_tcp:close(element(2, Pair))
  after 3000 ->
      log("timeout ~p <=> ~p ~n", [ClientSocket, ServerSocket]),
      {value, Pair} = lists:keysearch(ClientSocket, 1, SocketsTable),
      ok = gen_tcp:close(element(1, Pair)),
      ok = gen_tcp:close(element(2, Pair))
  end.

loop(SocketsTable, Listen) ->
  {ok, ClientSocket} = gen_tcp:accept(Listen, infinity),
  Pid = spawn(fun() -> conversation(ClientSocket, SocketsTable) end),
  ok=gen_tcp:controlling_process(ClientSocket, Pid),
  loop(SocketsTable, Listen).

main() ->
  inets:start(),
  SocketsTable = [],
  {ok, Listen} = gen_tcp:listen(?PORT, ?TCP_OPTS),
  loop(SocketsTable, Listen).
