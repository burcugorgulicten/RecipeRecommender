
% zip(L1,L2,L3) is true if L3 is
% array of array with L1's and L2's
% corresponding elements
% L1 and L2 needs to have the same length
% L1: 1d array
% L2: 1d array
% L3: 2d array
zip([], [], []).
zip([H1|T1], [H2|T2], [[H1,H2]|T3]) :- 
    zip(T1,T2,T3).


% zip2(L1,L2,L3) is true if L3 is
% array of array with L1's elements
% added to L2 corresponding arrays
% L1: 1d array
% L2: 2d array
% L3: 2d array
zip2([], [], []).
zip2([H1|T1], [H2|T2], [[H1|H2]|T3]) :- 
    zip2(T1,T2,T3).


% append(L1, L2, L3) is true if L3 is
% L3 is L1 appended to L2
append([], L, L).        
append([X|Y], L, Z):-
    append(Y, [X|L], Z).

%% ---------------------------------------------------------------
%% from: https://stackoverflow.com/questions/20256667/prolog-removing-duplicates/20264879

 % An empty list is a set.
set([], []).

% Put the head in the result,
% remove all occurrences of the head from the tail,
% make a set out of that.
set([H|T], [H|T1]) :- 
    remv(H, T, T2),
    set(T2, T1).

% Removing anything from an empty list yields an empty list.
remv(_, [], []).

% If the head is the element we want to remove,
% do not keep the head and
% remove the element from the tail to get the new list.
remv(X, [X|T], T1) :- remv(X, T, T1).

% If the head is NOT the element we want to remove,
% keep the head and
% remove the element from the tail to get the new tail.
remv(X, [H|T], [H|T1]) :-
    X \= H,
    remv(X, T, T1).
%% ---------------------------------------------------------------