%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency2).
-export([start/0,allocate/0,deallocate/1,inject/1,stop/0]).
-export([init/0, loop/1]).

-ifdef(can_inject).
-endif.

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(?MODULE,
	     spawn(?MODULE, init, [])).

init() ->
  Frequencies = {get_frequencies(), []},
  ?MODULE:loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  io:format("loop ~w~n",[Frequencies]),
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      ?MODULE:loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      ?MODULE:loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped};
    {request, Pid , Unknown} ->
      {NewFrequencies, Reply} = unknown_request(Frequencies, Pid, Unknown),
      Pid ! {reply, Reply},
      ?MODULE:loop(NewFrequencies)
  end.

%% Functional interface

allocate() -> 
    ?MODULE ! {request, self(), allocate},
    receive 
	    {reply, Reply} -> Reply
    end.

deallocate(Freq) -> 
    ?MODULE ! {request, self(), {deallocate, Freq}},
    receive 
	    {reply, Reply} -> Reply
    end.

-ifdef(can_inject).

inject(Freqs) ->
    ?MODULE ! {request, self(), {inject, Freqs}},
    receive
	    {reply, Reply} -> Reply
    end.

-else.

inject(_) -> {reply, error}.

-endif.

stop() -> 
    ?MODULE ! {request, self(), stop},
    receive 
	    {reply, Reply} -> Reply
    end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

-ifdef(can_inject).
unknown_request(Frequencies, _Pid, {inject, NewFreqs}) -> {inject(Frequencies, NewFreqs), ok};
unknown_request(Frequencies, _Pid, _Unknown) -> {Frequencies, undef}.
-else.
unknown_request(Frequencies, _Pid, _Unknown) -> {Frequencies, undef}.
-endif.


-ifdef(can_inject).
inject(Existing, []) ->
  io:format("inject ~w ~n",[Existing]),
  Existing;
inject({Free, Allocated}, [NewFreq|NewFreqs]) ->
  io:format("inject ~w ~w~n",[NewFreq, NewFreqs]),
  inject({[NewFreq|Free],  Allocated}, NewFreqs).
-endif.
