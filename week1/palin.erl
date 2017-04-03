-module(palin).
-export([palin/1,nopunct/1,palindrome/1,server/1]).

% palindrome problem
%
% palindrome("Madam I\'m Adam.") = true

palindrome(Xs) ->
    palin(nocaps(nopunct(Xs))).

nopunct([]) ->
    [];
nopunct([X|Xs]) ->
    case lists:member(X,".,\ ;:\t\n\'\"") of
	true ->
	    nopunct(Xs);
	false ->
	    [ X | nopunct(Xs) ]
    end.

nocaps([]) ->
    [];
nocaps([X|Xs]) ->
    [ nocap(X) | nocaps(Xs) ].

nocap(X) ->
    case $A =< X andalso X =< $Z of
	true ->
	    X+32;
	false ->
	    X
    end.

% literal palindrome

palin(Xs) ->
    Xs == reverse(Xs).

reverse(Xs) ->
    shunt(Xs,[]).

shunt([],Ys) ->
    Ys;
shunt([X|Xs],Ys) ->
    shunt(Xs,[X|Ys]).

%Define a function server/1 that accepts messages of the form
%{check,"Madam I\'m Adam"}
%and returns results like
%{result,"\"Madam I\'m Adam\" is a palindrome"}
%If it is sent any other format of message, such as
%stop
%the server should stop, by terminating its operation.
%The argument to server/1 should be the Pid of the process to which results are to be returned.
	
server(Return) ->
    receive
    {check, String} ->
        Return ! {result, isaPalindrome(String)},
        server(Return);
    stop -> null
    end.

isaPalindrome(String) ->
    String ++
    (case palindrome(String) of false -> " is not"; true -> " is" end) ++
    " a palindrome".
