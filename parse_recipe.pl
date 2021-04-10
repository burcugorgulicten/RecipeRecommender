:- use_module(library(http/json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- ensure_loaded([list, write_recipes]).

recipeInformation('&addRecipeInformation=true').
numberOfResults('&number=1').

% for testing
% parses the json file eg to a prolog json atom,
% gets the recipes, and writes them to a file
get_dict_from_json_file(Recipes) :-
  open('./eg.json', read, Stream),
  json_read(Stream, json(JsonArr)),
  get_recipes(JsonArr, Recipes),
  write_to_file(Recipes),
  close(Stream).



% Makes the request with the given params and gets 
% the recipes, and writes them to a file
% try get_files(R, "&cuisine=italian").
get_files(Recipes, SearchString):-
    try_request(json(JsonArr), SearchString),
    get_recipes(JsonArr, Recipes),
    write_to_file(Recipes).



% get_recipes(JSON, R) is true if R is
% the list of recipes with 
% Recipe Name
% Ingredients
% Instructions
% Prep time
% url for more info
% given the json object JSON
get_recipes(JsonArr, Recipes):-
    get_results(JsonArr, R),
    get_names(R, Names),
    get_times(R, Times),
    get_infoLinks(R, Infolinks),
    get_ingredientLists(R, Ingredients),
    get_instructionsList(R, Instructions),
    zip(Instructions, Infolinks, Recipes1),
    zip2(Ingredients, Recipes1, Recipes2),
    zip2(Times, Recipes2, Recipes3),
    zip2(Names, Recipes3, Recipes).



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



% get_infoLinks(L,N) is true if N
% is the list of URLs in the array L
% i,e,. L is list of recipes and N is
% list of sourceUrl of the recipes
get_infoLinks([], []).

get_infoLinks([json(H)|T], [Link|Links]):-
    get_infoLink(H, Link),
    get_infoLinks(T,Links).


% get_infoLink([H|T], N) is true if H has
% the element "sourceUrl" in the array
% and N is the value of the url
% i,e,. the array is one json object (one recipe)
% and sourceUrl is the url of the given
% recipe
get_infoLink([sourceUrl=N|_], more_info(N)).

get_infoLink([_|T], N) :-
    get_infoLink(T,N).


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




% step_instruction(L, N) is true if 
% N is the instruction given the
% current step
step_instruction([step=N|_], N).

step_instruction([_|T], N) :-
    step_instruction(T,N).



% step_instructions(L1, L2) is true if 
% L2 is the list of instructions in the
% given analyzedInstructions L1
step_instructions([], []).

step_instructions([json(H)|T], [I|Is]):-
    step_instruction(H, I),
    step_instructions(T,Is).



% get_instructionsList(L1, L2) is true if L2 is 
% the list of instructions of the given 
% list of recipes L1
get_instructionsList([], []).

get_instructionsList([json(H)|T], [Instruction|Instructions]):-
    get_instructions(H, Instruction),
    get_instructionsList(T,Instructions).



% get_instructions(L, instructions(S)) is true if S is the 
% instructions in the given recipe L
get_instructions([analyzedInstructions=[]|_], instructions("No Information")).

get_instructions([analyzedInstructions=[json(N)]|_], instructions(Ins)):-
    get_steps(N, S),
    step_instructions(S, Ins).

get_instructions([_|T], N) :-
    get_instructions(T,N).


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
    set(IN, UniqueIngs).

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



% try_request(JSON, SearchString) tries to
% make an api request with the SearchString
% If successful returns the JSON, if not
% prints the error out and exits.
% try try_request(JSON, "asdf"). to make
% it catch an error
try_request(JSON, SearchString):-
    catch(
        make_request(JSON, SearchString),
        _,
        false).


% try make_request(JSON, "&maxFat=25").
make_request(JSON, SearchString):-
    apiKey(Key),
    recipeInformation(AddRecipeInfo),
    numberOfResults(No),
    atom_concat('https://api.spoonacular.com/recipes/complexSearch?apiKey=', Key, URLwithKey),
    atom_concat(URLwithKey, AddRecipeInfo, URLwithInfo),
    atom_concat(URLwithInfo, No, URLwithNo),
    atom_concat(URLwithNo, SearchString, URL),
    http_get(URL,
            JSON, 
            []).
