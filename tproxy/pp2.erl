-module(pp2).
-author("<edbond@gmail.com>").
-vsn(1.0).

-define(PORT, 3456).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([main/0]).

code_change(_OldVsn, _State, _Extra) -> ok.
handle_call(_Request, _From, _State) -> ok.
handle_cast(_Request, _State) -> ok.
handle_info(_Info, _State) -> ok.
terminate(_Reason, _State) -> ok.

read_until(Stop, Socket, Acc) ->
  io:format("read_until ~p ~p ~p~n", [Stop, Socket, Acc]),
  {ok, Data} = gen_tcp:recv(Socket, 0),
  io:format("read ~p~n", [Data]),
  NewAcc = [Acc | Data],
  Pos = string:str(binary_to_list(Data), Stop),
  if
    Pos == 0 ->
      io:format("read more...~n", []),
      read_until(Stop, Socket, NewAcc);
    true ->
      io:format("zzzz~n", [])
  end,
  io:format("returning ~p~n", [NewAcc]),
  lists:flatten(NewAcc).

init(Args) ->
  [ClientSocket] = Args,
  io:format("init: ~p~n", [ClientSocket]),
  Request = read_until("\r\n\r\n", ClientSocket, []),
  io:format("got request: ~p~n", [Request]),
  {ok, state}.

loop(ListenSocket) ->
  {ok, ClientSocket} = gen_tcp:accept(ListenSocket, infinity),
  io:format("connection ~p~n", [ClientSocket]),
  _Proxy = gen_server:start_link(?MODULE, [ClientSocket], []),

  loop(ListenSocket).

main() ->
  inets:start(),
  % listen to socket

  {ok, Socket} = gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, false}]),
  io:format("listen ~p~n", [Socket]),

  loop(Socket).
