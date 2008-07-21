-module(gitpull).
-author(edbond@gmail.com).
-vsn(1.0).
-export([main/0]).
-import(filelib, [is_dir/1, wildcard/1]).
-import(filename, [join/1]).
-import(lists, [filter/2]).
-import(timer, [sleep/1]).
-import(io, [format/2]).
-import(io_lib).

collectOutput(Port, Output) ->
  receive
    {Port, {data, Data}} ->
      io:format("receive data: ~s~n", [Data]),
      collectOutput(Port, [Output | Data]);
    {Port, {exit_status, _}} ->
      %% return collected output
      Output;
    {Port, Msg} ->
      io:format("receive unknown message from port: ~p~n", [Msg]),
      exit({unknown_message, Msg})
  end.

gitpull(Pid, Path) ->
  io:format("i'm going to git pull ~p~n", [Path]),
  Git = open_port({spawn, "git pull"}, [stream, {cd, Path}, binary, eof, exit_status]),

  Result = collectOutput(Git, []),

  io:format("sending result to ~p~n", [Pid]),
  Pid ! {ok, Path, Result}.


wait_for_output(0, Results) ->
  Results;
wait_for_output(N, Results) ->
  receive
    {ok, _, Result} ->
      wait_for_output(N-1, [Result | Results])
  end.

loop(Dirs) ->
  %% io:format("loop [~p|~p], ~p~n", [D, T, Results]),
  %% spawn a new process
  Pid = self(),
  lists:foreach(fun(D) -> spawn_link(fun() -> gitpull(Pid, D) end) end, Dirs),
  wait_for_output(length(Dirs), []).

main() ->
  %%
  %% find git directories
  GitFilter = fun(F) -> filelib:is_dir(F) and filelib:is_dir(filename:join([F,".git"])) end,

  GitDirs = lists:filter(GitFilter, filelib:wildcard("*")),
  io:format("git dirs: ~p~n", [GitDirs]),
  %%
  loop(GitDirs),
  init:stop().
