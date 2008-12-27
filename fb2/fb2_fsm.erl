-module(fb2_fsm).
-behaviour(gen_fsm).
-compile(export_all).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([initialized/3, book_title/3]).

-record(fb2_info, {title, description, filename, firstname, lastname}).

% {ok,StateName,StateData}
init(_Args) -> {ok, initialized, #fb2_info{}}.
handle_event(_Event, _StateName, StateData) -> {next_state, init, StateData}.
handle_sync_event(_Event, _From, StateName, StateData) -> {reply,ok,StateName,StateData}.
handle_info(_Info, StateName, StateData) -> {next_state, StateName, StateData}.
terminate(_Reason, _StateName, _StateData) -> bye.
code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.

%% states
initialized(Event, _From, StateData) ->
  %io:format("new event: ~p~n", [Event]),
  case Event of
    {element, "first-name"} ->
      {reply, ok, firstname, StateData};
    {element, "last-name"} ->
      {reply, ok, lastname, StateData};
    {element, "book-title"} ->
      {reply, ok, book_title, StateData};
    _Other ->
      {reply, ok, initialized, StateData}
  end.
  %{next_state, initialized, StateData}.

firstname(Event, _From, StateData) ->
  io:format("firstname event: ~p ~p ~n", [Event, StateData]),
  case Event of
    {characters, Chars} ->
      NewStateData = StateData#fb2_info{firstname=Chars},
      {reply, ok, firstname, NewStateData};
    {end_element, "first-name"} ->
      {reply, ok, initialized, StateData}
  end.

lastname(Event, _From, StateData) ->
  io:format("lastname event: ~p ~p ~n", [Event, StateData]),
  case Event of
    {characters, Chars} ->
      NewStateData = StateData#fb2_info{lastname=Chars},
      {reply, ok, lastname, NewStateData};
    {end_element, "last-name"} ->
      {reply, ok, initialized, StateData}
  end.

book_title(Event, _From, StateData) ->
  io:format("book title event: ~p ~p ~n", [Event, StateData]),
  case Event of
    {characters, Chars} ->
      NewStateData = StateData#fb2_info{title=Chars},
      {reply, ok, book_title, NewStateData};
    {end_element, "book-title"} ->
      {reply, ok, initialized, StateData}
  end.
