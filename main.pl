:- ensure_loaded([parse_recipe, cfg]).

% Entry point
q(N, SearchString) :-
    write("Enter recipe query: "), flush_output(current_output),
    readln(Ln),
    once(get_search_string(Ln, SearchString)),
    once(get_files([H|_], SearchString)),
    member(name(N), H).

% for testing (makes no requests)
q_test(SearchString) :-
    write("Enter recipe query: "), flush_output(current_output),
    readln(Ln),
    once(get_search_string(Ln, SearchString)).

% get_search_string(Q,S) is true if C is a search string with constraints from question Q
get_search_string(Q,S) :-
    question(Q,End,C,[]),
    member(End,[[],['?']]),
    combine(C,S).

% combine(Lst, S) is true if S is the concatentation of all atoms in Lst
combine([],'').
combine([H|T],S) :-
    combine(T,TS),
    atom_concat(H,TS,S).