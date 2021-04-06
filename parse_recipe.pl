:- use_module(library(http/json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- ensure_loaded([list]).


% for testing
% parses the json file to a prolog json atom
get_dict_from_json_file(Recipes) :-
  open('./eg.json', read, Stream),
  json_read(Stream, json(JsonArr)),
  get_results(JsonArr, R),
  get_names(R, Names),
  get_times(R, Times),
  get_ingredientLists(R, Ingredients),
  zip(Ingredients, Times, Recipes1),
  zip2(Names, Recipes1, Recipes),
  close(Stream).



% Makes the request with the given params and gets 
% the recipe names
% try get_files(R, "&cuisine=italian&number=1&addRecipeInformation=true").
get_files(Recipes, SearchString):-
    make_request(json(JsonArr), SearchString),
    get_results(JsonArr, R),
    get_names(R, Names),
    get_times(R, Times),
    get_ingredientLists(R, Ingredients),
    zip(Ingredients, Times, Recipes1),
    zip2(Names, Recipes1, Recipes).


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
get_name([title=N|_], name(N)).

get_name([_|T], N) :-
    get_name(T,N).





% get_times(L,T) is true if T
% is the list of prep times in the array L
% i,e,. L is list of recipes and T is
% list of prep times of the recipes
get_times([], []).

get_times([json(H)|T], [Time|Times]):-
    get_time(H, Time),
    get_times(T,Times).


% get_time([H|T], N) is true if H has
% the element "readyInMinutes" in the array
% and N is the value of readyInMinutes
% i,e,. the array is one json object (one recipe)
% and title is the recipe's prep time
get_time([readyInMinutes=N|_], time(N)).

get_time([_|T], N) :-
    get_time(T,N).



% get_steps(L1,L2) is true if L2
% is the steps of the recipe L1
get_steps([],[]).

get_steps([steps=N|_], N).

get_steps([_|T], N) :-
    get_steps(T,N).


% step_ingredient(L1, N) is true if
% N is the ingredients in the step L1
step_ingredient([ingredients=N|_], N).

step_ingredient([_|T], N) :-
    step_ingredient(T,N).


% step_ingredients(L1, L2) is true if L2 is
% the ingredients from list of steps L1 in
% a recipe
step_ingredients([], []).

step_ingredients([json(H)|T], FinalIng):-
    step_ingredient(H, I),
    step_ingredients(T,Is),
    append(I, Is, FinalIng).

% get_ingredientLists(L1, L2) is true if L2
% is the ingredient lists from L1
get_ingredientLists([], []).

get_ingredientLists([json(H)|T], [I|Is]):-
    get_ingredients(H, I),
    get_ingredientLists(T,Is).


% get_ingredients(L1, ingredients(L2)) is true if L2 is the ingredients
% of the recipe L1
get_ingredients([analyzedInstructions=[]|_], ingredients("No Information")).

get_ingredients([analyzedInstructions=[json(N)]|_], ingredients(UniqueIngs)):-
    get_steps(N, S),
    step_ingredients(S, I),
    get_ingredient_names(I, IN),
    set(IN, UniqueIngs),
    write(UniqueIngs).

get_ingredients([_|T], N) :-
    get_ingredients(T,N).

% get_ingredient_names(L1, L2) is true if L2 is the names
% of the ingredients given the list of json ingredients L1
get_ingredient_names([], []).

get_ingredient_names([json(H)|T], [I|Is]):-
    find_name(H, I),
    get_ingredient_names(T,Is).


% find_name(L, N) is true if N is the value of
% name (an element of the array) given an array
find_name([name=N|_], N).
find_name([_|T], N) :-
    find_name(T,N).


% try make_request(JSON, "&maxFat=25&number=1&addRecipeInformation=true").
make_request(JSON, SearchString):-
    apiKey(Key),
    atom_concat('https://api.spoonacular.com/recipes/complexSearch?apiKey=', Key, URLwithKey),
    atom_concat(URLwithKey, SearchString, URL),
    http_get(URL,
            JSON, 
            []).
