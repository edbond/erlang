%#!/usr/bin/env escript
-module(endsrename).
-author('Eduard Bondarenko <edbond@gmail.com>').

-mode(compile).
-compile( [ native, { hipe, o3 } ] ).
-compile( [ inline, { inline_size, 100 } ] ).
-export([main/1]).

cleanup_title(C,A) when (C>64) and (C<123) -> A++[C];
cleanup_title(C,A) when (C>47) and (C<58) -> A++[C];
cleanup_title(_C, A) ->
  %% squeeze
  L=lists:last(A),
  case L of
    $_ -> A;
    _ -> A++[$_]
  end.

rename_save(Romname, CleanRomname) ->
  Saves = lists:map(fun(F) -> string:to_lower(filename:basename(F)) end, filelib:wildcard("../SAVE/*.*")),
  SaveFilenames = lists:map(fun(S) -> filename:rootname(S) end, Saves),

  %% find savename that matches original romname
  Savename = lists:filter(fun(S) -> Romname == S end, SaveFilenames),
  case length(Savename) of
    0 -> Cmd = void;
    _ -> Cmd = "mv '../SAVE/"++hd(Savename)++".sav' '../SAVE/"++CleanRomname++".sav'"
  end,
  Cmd.

rename_rom(Filename) ->
  Romname = filename:rootname(Filename),
  {ok, FH}=file:open(Filename, [read,binary]),

  {ok, <<Offset:32/little>>}=file:pread(FH, 16#68, 4),
  {ok, <<UCSTitle/binary>>}=file:pread(FH, Offset+4+28+512+32+256, 256),
  Title = lists:filter(fun(X) -> X /= 0 end, binary_to_list(UCSTitle)),
  CleanRomname = lists:foldl(fun(X, A) -> cleanup_title(X,A) end, [], Title),

  if Romname /= CleanRomname ->
    Cmd = "mv '"++Romname++".nds' '"++CleanRomname++".nds'",
    SaveCmd = rename_save(Romname,CleanRomname);
  true ->
    Cmd = void,
    SaveCmd = void
  end,
  [Cmd, SaveCmd].

output(void) -> true;
output(Cmd) -> io:format("~s~n", [Cmd]).

%% this is for parallel version
getoutput(R) ->
  leader ! {output, rename_rom(R)},
  exit(normal).

wait_loop(0) ->
  ok;
wait_loop(N) ->
  receive
    {output, Output} ->
      %% output
      lists:map(fun(C) -> output(C) end, Output),
      wait_loop(N-1)
  end.

main_loop() ->
  Roms = filelib:wildcard("*.nds"),

  %% parallel version
  register(leader, self()),
  lists:foreach(fun(R) -> spawn(fun() -> getoutput(R) end) end, Roms),
  wait_loop(length(Roms)).

  %% flat version
  %Cmds = lists:flatmap(fun(X) -> rename_rom(X) end, Roms),
  %% output
  %lists:map(fun(C) -> output(C) end, Cmds),

main(_) ->
  {Ms, _Value} = timer:tc(?MODULE, main_loop, []),
  io:format("# finished in ~p ms~n", [Ms]),
  halt().
