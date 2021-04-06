 :- use_module(library(http/json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).


% for testing
% parses the json file to a prolog json atom
get_dict_from_json_file(Names) :-
  open('./eg.json', read, Stream),
  json_read(Stream, json(JsonArr)),
  get_results(JsonArr, R),
  get_names(R, Names),
  close(Stream).


% Makes the request with the given params and gets 
% the recipe names
get_files(Names, SearchString):-
    make_request(json(JsonArr), SearchString),
    get_results(JsonArr, R),
    get_names(R, Names).


% get_results(L, R) is true if the json object L has 
% "results" array
% Gets the results from the response, list of recipes 
%(for prolog list of json())
get_results([results=A|_], A).

get_results([H|T], A):-
    dif(results=_, H),
    get_results(T,A).


% get_names(L,N) is true if N
% is the list of names in the array L
% i,e,. L is list of recipes and N is
% list of names of the recipes
get_names([], []).

get_names([json(H)|T], [Name|Names]):-
    get_name(H, Name),
    get_names(T,Names).



% get_name([H|T], N) is true if H has
% the element "title" in the array
% and N is the value of the title
% i,e,. the array is one json object (one recipe)
% and title is the recipe's name
get_name([title=N|_], N).

get_name([_|T], N) :-
    get_name(T,N).


% try make_request(JSON, "&maxFat=25&number=1&addRecipeInformation=true").
make_request(JSON, SearchString):-
    apiKey(Key),
    atom_concat('https://api.spoonacular.com/recipes/complexSearch?apiKey=', Key, URLwithKey),
    atom_concat(URLwithKey, SearchString, URL),
    http_get(URL,
            JSON, 
            []).
