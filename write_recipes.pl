% writes the Recipes in a text 
% file called 'recipes'
write_to_file(Recipes) :-
    open('recipes.txt',write, Stream),
    write_recipes(Stream, Recipes),
    close(Stream).


% Iterates through the recipes
% to write them individually
write_recipes(_, []).

write_recipes(Stream, [H|T]):-
    write_recipe(Stream, H),
    write(Stream, "\n"),
    write(Stream, "\n"),
    write_recipes(Stream, T).


% write_recipe(Stream, L) is true if L
% is the list of a single recipe with
% its info and is written in the file
% by Stream
write_recipe(_, []).
write_recipe(Stream, [H|T]):- 
    write_feature(Stream, H),
    write(Stream, "\n"),
    write_recipe(Stream, T).


% write_feature(Stream, N) is true if 
% N is one of 
% name()
% more_info()
% instructions()
% time()
% ingredients()
% and writes that feature in the file
write_feature(Stream, name(N)):-
    write(Stream, "TITLE: "),
    write(Stream, N).


write_feature(Stream, more_info(N)):-
    write(Stream, "MORE INFO: "),
    write(Stream, N).

write_feature(Stream, instructions(N)):-
    write(Stream, "INSTRUCTIONS: "),
    write_instructions(Stream, N, 1).


write_feature(Stream, time(N)):-
    write(Stream, "Takes "),
    write(Stream, N),
    write(Stream, " minutes").

write_feature(Stream, ingredients(N)):-
    write(Stream, "INGREDIENTS: "),
    write_ingredients(Stream, N).


% write_ingredients(Stream, L) is true if L
% is either a list or "No Information"
% if it's a list then it writes every ingredient
% on a new line in the file
write_ingredients(Stream, "No Information"):-
    write(Stream, "No Information").

write_ingredients(_, []).

write_ingredients(Stream, [H|T]):-
    write(Stream, "\n"),
    write(Stream, "・ "),
    write(Stream, H),
    write_ingredients(Stream, T).


% write_instructions(Stream, L) is true if L
% is either a list or "No Information"
% if it's a list then it writes every instruction
% on a new line with its index
write_instructions(Stream, "No Information", _):-
    write(Stream, "No Information").

write_instructions(_, [], _).

write_instructions(Stream, [H|T], Ind):-
    write(Stream, "\n"),
    write(Stream, Ind),
    write(Stream, ") "),
    write(Stream, H),
    NewInd is Ind + 1,
    write_instructions(Stream, T, NewInd).