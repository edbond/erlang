-module(gui).
-export([init/0]).

init() ->
  S = gs:start(),
  Win = gs:create(window, S, [{width, 350}, {height, 100}]),
  gs:create(button, quit, Win, [{label, {text, "Quit"}}, {x, 0}]),
  gs:create(button, spawn, Win, [{label, {text, "Spawn"}}, {x, 100}]),
  gs:create(button, error, Win, [{label, {text, "Error"}}, {x, 200}]),
  gs:config(Win, {map, true}),
  loop().

loop() ->
  receive
    {gs, spawn, click, _, _} ->
      Pid = spawn(?MODULE, init, []),
      handle_error(self(), Pid),
      io:format("got here~n",[]),
      loop();
    {gs, quit, click, _, _} ->
      io:format("quitting~n",[]),
      gs:stop();
    {gs, error, click, _, _} ->
      erlang:error(errorclick);
    exit ->
      bye
  end.

handle_error(MasterPid, Pid) ->
  spawn(fun() ->
        process_flag(trap_exit, true),
        link(Pid),
        receive 
          {'EXIT', Pid, {errorclick, _}} ->
            io:format(" ~p died :~n",[Pid]),
            MasterPid ! {gs, spawn, click, a, b};
          _ ->
            io:format(" really quitting :~n", []),
            MasterPid ! exit
        end
    end).
