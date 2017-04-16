-module(freqclient).
-export([client/1, start_clients/1]).

% implement a client function that, when spawned as a process, can be used to model a client.
% To allow a simulation to go on indefinitely, your function should cycle through a number of
% allocations and deallocations, forever (since the server is designed to live indefinitely, too).
% We would like to model systems where there are multiple clients, so it would be useful to
% parameterise the client so that it can behave in different ways when called with different parameters.

client(Interval) ->
    process_flag(trap_exit, true),
    client_loop(Interval).

client_loop(Interval) ->
    process_flag(trap_exit, true),
    timer:sleep(rand:uniform(Interval)),
    case frequency:allocate() of
        {ok, Freq} ->
            timer:sleep(rand:uniform(Interval)),
            frequency:deallocate(Freq),
            client_loop(Interval);
        {error, Msg} ->
            io:format("~w~n", Msg),
            client_loop(Interval);
        {'EXIT', FromPid, Reason} ->
            io:format("Exit ~w: ~w ~n", [FromPid|Reason]),
            client_loop(Interval)
    end.



start_clients(Intervals) ->
    lists:map(fun(Interval)->spawn(freqclient,client,[Interval]) end, Intervals).
