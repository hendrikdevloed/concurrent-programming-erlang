%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency2).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

-define(DEBUG_CALL(Args), (fun() -> DEBUG_CALL = (Args), error_logger:info_msg("~w:~w -> ~s:~n ~150p~n", [?MODULE, ?LINE, ??Args, DEBUG_CALL]), DEBUG_CALL end)()).


start() ->
    register(?MODULE,
	     spawn(?MODULE, init, [])).

init() ->
  Servers = lists:map(
    fun(Instance) ->
      spawn_link(fun() -> init(Instance) end)
    end,
    [1,2]
  ),
  router_loop(Servers, 0).

init(Instance) ->
  Frequencies = {get_frequencies(Instance), []},
  loop(Frequencies).

% Hard Coded
get_frequencies(Instance) -> lists:map(
  fun(X)->X+10*Instance end,
  [0,1,2,3,4,5]
).

%% The Main Loop

router_loop(Servers, LastAlloc) ->
  receive
    {request, Pid, allocate} ->
        ?DEBUG_CALL(lists:nth(1+LastAlloc, Servers) ! {request, Pid, allocate}),
        router_loop(Servers, (LastAlloc+1) rem length(Servers));
    {request, Pid, {deallocate, Freq}} ->
        lists:nth(Freq div 10, Servers) ! {request, Pid, {deallocate, Freq}},
        router_loop(Servers, LastAlloc);
    {request, Pid, stop} ->
      lists:map(
        fun(Server) ->
          Server ! {request, Pid, stop}
        end,
        Servers
      )
  end.

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
      Pid ! {reply, stopped}
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
