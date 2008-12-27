-module(fb2_fsm).
-behaviour(gen_fsm).
-compile(export_all).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([initialized/3, book_title/3]).

-record(fb2_info, {author, title, filename, lastname}).

% {ok,StateName,StateData}
init(_Args) ->
  {ok, initialized, #fb2_info{}}.

% Module:handle_event(Event, StateName, StateData) -> Result
% Result = {next_state,NextStateName,NewStateData}
handle_event(Event, _StateName, StateData) ->
  io:format("event: ~p~n", [Event]),
  {next_state, init, StateData}.

% Module:handle_sync_event(Event, From, StateName, StateData) -> Result
% Result = {reply,Reply,NextStateName,NewStateData}
handle_sync_event(Event, _From, _StateName, StateData) ->
  Reply=ok,
  NextStateName=author,
  io:format("sync_event: ~p~n", [Event]),
  {reply,Reply,NextStateName,StateData}.

% Module:handle_info(Info, StateName, StateData) -> Result
% Result = {next_state,NextStateName,NewStateData}
handle_info(Info, StateName, StateData) ->
  io:format("info: ~p~n", [Info]),
  {next_state, StateName, StateData}.

% Module:terminate(Reason, StateName, StateData)
terminate(Reason, _StateName, _StateData) ->
  io:format("terminate: ~p~n", [Reason]),
  bye.

% Module:code_change(OldVsn, StateName, StateData, Extra) -> {ok, NextStateName, NewStateData}
code_change(_OldVsn, StateName, StateData, _Extra) ->
  io:format("code change~n"),
  {ok, StateName, StateData}.


%% states
initialized(Event, _From, StateData) ->
  %io:format("new event: ~p~n", [Event]),
  case Event of
    {element, "last-name"} ->
      {reply, ok, lastname, StateData};
    {element, "book-title"} ->
      {reply, ok, book_title, StateData};
    Other ->
      {reply, ok, initialized, StateData}
  end.
  %{next_state, initialized, StateData}.

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
