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
  {ok, Data} = gen_tcp:recv(Socket, 0),

init(Args) ->
  [ClientSocket] = Args,
  Request = read_until("\r\n\r\n", ClientSocket, []),
  {ok, state}.

loop(ListenSocket) ->
  %Proxy = gen_server:start_link(?MODULE, [], []),
  %io:format("server process: ~p~n",[Proxy]),

  {ok, ClientSocket} = gen_tcp:accept(ListenSocket, infinity),
  Proxy = gen_server:start_link(?MODULE, [ClientSocket], []),

  loop(ListenSocket).

main() ->
  inets:start(),
  % listen to socket

  {ok, Socket} = gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, once}]),

  loop(Socket).
