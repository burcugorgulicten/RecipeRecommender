:- ensure_loaded([parse_recipe, cfg]).

% Entry point
q(N) :-
    write("Enter recipe query: "), flush_output(current_output),
    readln(Ln),
    get_lower_case(Ln, LC),
    remove_commas(LC, Q),
    once(get_search_string(Q, SearchString)),
    once(get_files(Recipes, SearchString)),
    read_names(Recipes, N).


q_test_JSON(Names, SearchString):-
    write("Enter recipe query: "), flush_output(current_output),
    readln(Ln),
    get_lower_case(Ln, LC),
    remove_commas(LC, Q),
    once(get_search_string(Q, SearchString)),
    once(get_dict_from_json_file(Recipes)),
    read_names(Recipes, Names).


% for testing (makes no requests)
q_test(SearchString) :-
    write("Enter recipe query: "), flush_output(current_output),
    readln(Ln),
    get_lower_case(Ln, LC),
    remove_commas(LC, Q),
    once(get_search_string(Q, SearchString)).

% get_search_string(Q,S) is true if C is a search string with constraints from question Q
get_search_string(Q,S) :-
    question(Q,End,C,[]),
    member(End,[[],['?'],['.']]),
    combine(C,S).

% combine(Lst, S) is true if S is the concatentation of all atoms in Lst
combine([],'').
combine([H|T],S) :-
    combine(T,TS),
    atom_concat(H,TS,S).


% get the names form the list of recipes
% read_names(R, N) is true if R is list of
% recipes and N is the names of the recipes
read_names([], []).
read_names([H|T], [N|Ns]):-
    read_name(H, N),
    read_names(T, Ns).

% read_name(L, N) is true if L is one recipe
% info and N is the name of the recipe
read_name([name(N)|_], N).
read_name([H|T], N):-
    dif(name(_), H),
    read_name(T, N).

% get_lower_case(L, LCs) is true if LCs
% is array L with the first atom lowercase
get_lower_case([], []).
get_lower_case([H|T], [LC|T]):-
    downcase_atom(H, LC).

% remove commas from list
remove_commas([],[]).
remove_commas([','|T],R) :-
    remove_commas(T,R).
remove_commas([H|T],[H|R]) :-
    dif(',',H),
    remove_commas(T,R).