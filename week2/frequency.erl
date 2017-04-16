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
  case whereis(frequency) of
    undefined ->
      spawn(fun start_server/0),
      ok;
    _Pid -> {error, "Already started"}
  end.

start_server() ->
  process_flag(trap_exit, true),
  start_server(0, 10).

start_server(Retry, RetriesPerSec) ->
  if
    Retry>RetriesPerSec -> exit(failed, "Too many failures");
    true ->
      register(frequency,
        spawn_link(frequency, init, [])),
      monitor_server(Retry, RetriesPerSec)
  end.

monitor_server(Retry, RetriesPerSec) ->
  % Only use a 1-second "after" timeout when we are retrying
  if 
    Retry==0 -> Timeout = infinity;
    true -> Timeout = 1000
  end,
  io:format("Monitoring frequency server (timeout ~w)~n",[Timeout]),
  receive
    {'EXIT', _FromPid, Reason} ->
      case Reason of
        stopped ->
          io:format("Stopped~n"),
          ok;
        _other ->
          NewRetry = Retry+1,
          io:format("Restart #~w~n",[NewRetry]),
          start_server(NewRetry, RetriesPerSec)
      end
  after
    Timeout ->
      % We had 1 uninterrupted second without crashes, reset retry counter
      % and continue waiting for process exit
      monitor_server(0, RetriesPerSec)  
  end.

init() ->
  process_flag(trap_exit, true),    %%% ADDED
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {request, Pid, stop} ->
    Pid ! {reply, stopped},
    exit(stopped);
    {'EXIT', Pid, _Reason} ->                   %%% CLAUSE ADDED
      NewFrequencies = exited(Frequencies, Pid), 
      loop(NewFrequencies)
  end.

%% Functional interface

allocate() -> 
    frequency ! {request, self(), allocate},
    receive 
	    {reply, Reply} -> Reply
    end.

deallocate(Freq) -> 
    frequency ! {request, self(), {deallocate, Freq}},
    receive 
	    {reply, Reply} -> Reply
    end.

stop() -> 
    frequency ! {request, self(), stop},
    receive 
	    {reply, Reply} -> Reply
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  link(Pid),                                               %%% ADDED
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  {value,{Freq,Pid}} = lists:keysearch(Freq,1,Allocated),  %%% ADDED
  unlink(Pid),                                             %%% ADDED
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

exited({Free, Allocated}, Pid) ->                %%% FUNCTION ADDED
    case lists:keysearch(Pid,2,Allocated) of
      {value,{Freq,Pid}} ->
        NewAllocated = lists:keydelete(Freq,1,Allocated),
        {[Freq|Free],NewAllocated}; 
      false ->
        {Free,Allocated} 
    end.

