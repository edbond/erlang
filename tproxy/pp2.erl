-module(pp2).
-author("<edbond@gmail.com>").
-vsn(1.0).

-define(PORT, 3456).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([main/0]).

-record(tp_state,
  {
    client_socket,
    server_socket
  }
).

code_change(_OldVsn, _State, _Extra) -> ok.
handle_call(_Request, _From, _State) -> ok.
handle_cast(_Request, _State) -> ok.
terminate(_Reason, _State) -> ok.

handle_info({tcp, From, Data}, State) ->
  ClientSocket = State#tp_state.client_socket,
  ServerSocket = State#tp_state.server_socket,

  case From of
    ClientSocket -> 
      gen_tcp:send(State#tp_state.server_socket, Data);
    ServerSocket -> 
      gen_tcp:send(State#tp_state.client_socket, Data)
  end,
  {noreply, State};

handle_info({tcp_closed, _From}, State) ->
  ok=gen_tcp:close(State#tp_state.client_socket),
  ok=gen_tcp:close(State#tp_state.server_socket),
  {noreply, State}; % or stop?

handle_info({tcp_error, _From, Reason}, State) ->
  io:format("tcp error: ~p~n", [Reason]),
  ok=gen_tcp:close(State#tp_state.client_socket),
  ok=gen_tcp:close(State#tp_state.server_socket),
  {noreply, State}; % or stop?

handle_info(Info, _State) ->
  io:format("info! ~p~n", [Info]),
  ok.

read_until(Stop, Socket, Acc) ->
  %io:format("read_until ~p ~p ~p~n", [Stop, Socket, Acc]),
  {ok, Data} = gen_tcp:recv(Socket, 0),
  %io:format("read ~p~n", [Data]),
  NewAcc = [Acc | Data],
  Pos = string:str(Data, Stop),
  if
    Pos == 0 ->
      %io:format("read more...~n", []),
      read_until(Stop, Socket, NewAcc);
    true ->
      %io:format("all~n", []),
      ok
  end,
  %io:format("returning ~p~n", [NewAcc]),
  lists:flatten(NewAcc).

init(Args) ->
  [ClientSocket] = Args,
  %io:format("init: ~p~n", [ClientSocket]),
  Request = read_until("\r\n\r\n", ClientSocket, []),
  io:format("got request: ~p~n", [Request]),

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
  %io:format("connecting to ~p:~p~n", [Address,Port]),

  {ok, ServerSocket} = gen_tcp:connect(Address, Port, []),
  %io:format("socket to server ~p ~n",[ServerSocket]),

  gen_tcp:send(ServerSocket, Request),
  {ok, #tp_state{server_socket=ServerSocket, client_socket=ClientSocket}}.

loop(ListenSocket) ->
  {ok, ClientSocket} = gen_tcp:accept(ListenSocket, infinity),
  %io:format("connection ~p~n", [ClientSocket]),
  _Proxy = gen_server:start_link(?MODULE, [ClientSocket], []),

  loop(ListenSocket).

main() ->
  inets:start(),
  % listen to socket

  {ok, Socket} = gen_tcp:listen(?PORT, [list, {packet, 0}, {active, false}]),
  io:format("listen ~p~n", [Socket]),

  loop(Socket).
