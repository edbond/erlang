-module(gitpull).
-author(edbond@gmail.com).
-vsn(1.0).
-export([main/0]).
-import(filelib, [is_dir/1, wildcard/1]).
-import(filename, [join/1]).
-import(lists, [filter/2]).
-import(timer, [sleep/1]).
-import(io, [format/2]).

-define(MAXPROC, 2).

%% conditional spawn
%% spawn if have freeslots
cspawn(F, Args, P) ->
  case P of
    true -> {ok, spawn(self(), F, Args)};
    false ->
      io:format("sleep...~n"),
      timer:sleep(2000),
      cspawn(F, Args, P)
  end.

loop([], Results) ->
  Results;

loop([D|T], Results) ->
  %% spawn a new process
  cspawn(fun(X) -> io:format("~p~n",X) end, [D], fun() -> true end),
  receive
    {ok, Result} ->
      Res = [Result | Res],
      loop(N-1, Res)
  end.

main() ->
  %%
  %% find git directories
  GitFilter = fun(F) -> filelib:is_dir(F) and filelib:is_dir(filename:join([F,".git"])) end,

  GitDirs = lists:filter(GitFilter, filelib:wildcard("*")),
  io:format("git dirs: ~p~n", [GitDirs]),
  %%
  loop(GitDirs, []).
