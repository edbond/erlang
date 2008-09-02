-module(tproxy).
-author("<edbond@gmail.com>").
-vsn(1.0).

-behaviour(gen_server).

-define(PORT, 3456).
-define(MAX_CONNECTS, 3000).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

reverse_test_() -> [
    ?_assert([3,2,1]==lists:reverse([1,2,3]))
  ].

% server state ?
-record(tp_state,
  {
    port, % port connection accepted on
    listen, % socket I'm listen on
    socket % socket I talk to
  }
).

accept(ServerPid, State) ->
  case gen_tcp:accept(State#tp_state.listen, infinity) of
    {ok, Socket} -> 
      io:format("going into loop ~p~n", [Socket]),
      loop(Socket),
      %spawn(fun() -> loop(Socket) end),
      {reply, ok, State#tp_state{socket=Socket} };
    {error, timeout} ->
      io:format("restart accept~n"),
      accept(ServerPid, State)
  end.

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
  case Request of
    {accept, Pid} ->
      io:format("accept call from ~p ~p ~n", [?LINE, Pid]),
      accept(Pid, State),
      {reply, From, State};
    Other ->
      io:format("unknown request call ~p~n", [Other])
  end.

handle_cast(Request, State) ->
  io:format("handle_cast: ~p~n", [Request]),
  {reply, Request, State}.

handle_info(Info, State) ->
  io:format("handle_info: ~p~n", [Info]),
  {reply, Info, State}.

terminate(Reason, State) ->
  io:format("terminate: ~p~n", [Reason]),
  ko.

code_change(OldVsn, State, Extra) ->
  io:format("code_change: ~p~n", [OldVsn]),
  updated.

loop(Socket) ->
  io:format("loop ~n",[]),
  case gen_tcp:recv(Socket,0) of
    {ok, Data} ->
      io:format("read data ~p~n", [Data]),
      gen_tcp:send(Socket, Data),
      loop(Socket);
    {error, Reason} ->
      io:format("socket closed ~p~n", [Reason]);
    Other ->
      io:format("unknown recv result ~p~n", [Other])
  end.

accept_loop(Pid) ->
  gen_server:call(Pid, {accept, Pid}),
  %accept(Pid, {some_state}),
  accept_loop(Pid).

main() ->
  {ok, Pid} = gen_server:start_link(?MODULE, [], []),
  io:format("started gen_server pid: ~p~n", [Pid]),
  accept_loop(Pid).

  %inets:start(),
  %{ok, {{Version, 200, ReasonPhrase}, Headers, Body}} = http:request("http://www.erlang.org"),
  %io:format("yo! ~p~n", [Body]),
  %inets:stop().
