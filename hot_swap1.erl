-module(hot_swap1).
-compile(export_all).
-define(VERSION, 3).

-export([runner/0]).

start() ->
  Pid = spawn_link(?MODULE, runner, []),
  register(?MODULE, Pid),
  Pid.

runner() ->
  receive
    {echo, Msg} ->
      io:format("v~p Some dude said: ~p~n", [?VERSION, Msg]);
    _ ->
      ignore
  end,
  ?MODULE:runner().
