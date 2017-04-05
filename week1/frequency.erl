%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(frequency,
	     spawn(frequency, init, [])).

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].
% Set to true to simulate an overloaded server
debug_simulate_overload() -> false.

%% The Main Loop

loop(Frequencies) ->
  case debug_simulate_overload() of true -> timer:sleep(4000); false -> ok end,
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      {NewFrequencies, Reply} = deallocate(Frequencies, Freq, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% Functional interface

allocate() ->
    clear(), % Ensure no stale replies exist
    frequency ! {request, self(), allocate},
    receive 
	    {reply, Reply} -> Reply
    after 2000 -> {error, timeout}
    end.

deallocate(Freq) -> 
    clear(), % Ensure no stale replies exist
    frequency ! {request, self(), {deallocate, Freq}},
    receive 
	    {reply, Reply} -> Reply
    after 2000 -> {error, timeout}
    end.

stop() -> 
    clear(), % Ensure no stale replies exist
    frequency ! {request, self(), stop},
    receive 
	    {reply, Reply} -> Reply
    after 2000 -> {error, timeout}
    end.

% Flushing the mailbox
clear() ->
  receive
    Msg ->
      % Show what we're discarding if debugging
      case debug_simulate_overload() of
        true -> io:format("(Discarded old reply ~w)~n", [Msg]);
        false -> ok
      end,
      clear()
  after 0 -> ok
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  % Check if the requesting Pid has already allocated a frequency
  case lists:keyfind(Pid, 2, Allocated) of
    false -> % Allocate a new frequency
      {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}};
    _Found -> % Refuse request: quotum of 1 frequency per pid
      {{[Freq|Free], Allocated}, {error, quota_exceeded}}
  end.

deallocate({Free, Allocated}, Freq, Pid) ->
  % Check if the requesting Pid owns the frequency it is deallocating
  case lists:member({Freq, Pid}, Allocated) of
    false -> % Refuse deallocateion
      {{Free, Allocated}, {error, permission_denied}};
    true -> % Deallocate
      NewAllocated=lists:keydelete(Freq, 1, Allocated),
      {{[Freq|Free],  NewAllocated}, ok}
  end.

