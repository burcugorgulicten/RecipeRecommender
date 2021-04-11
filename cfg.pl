:- discontiguous s/6, s/4.  % suppress warnings
:- ensure_loaded(['wordnet/wn_s', 'wordnet/wn_hyp']).

% This code was initially based on Figure 13.12 of Poole and Mackworth, 
% Artificial Intelligence: foundations of computational agents, Cambridge, 2017

% question(Q,L,C0,C1) is true if C0-C1 is the constraints from question Q-L
question([what,is | L0],L1,C0,C1) :-
    noun_phrase(L0,L1,C0,C1).
question([what,is | L0],L3,C0,C3) :-
    noun_phrase(L0,L1,C0,C1),
    noun_phrase(L1,L2,C1,C2),
    verb_phrase(L2,L3,C2,C3).
question([what | L0],L4,C0,C4) :-
    noun_phrase(L0,L1,C0,C1),
    verb_phrase(L1,L2,C1,C2),
    noun_phrase(L2,L3,C2,C3),
    verb_phrase(L3,L4,C3,C4).
question(L0,L4,C0,C4) :-
    verb_phrase(L0,[for | L1],C0,C1),
    noun_phrase(L1,L2,C1,C2),
    noun_phrase(L2,L3,C2,C3),
    mp(L3,L4,C3,C4).
question(L0,L5,C0,C5) :-
    verb_phrase(L0,[for | L1],C0,C1),
    noun_phrase(L1,L2,C1,C2),
    noun_phrase(L2,L3,C2,C3),
    noun_phrase(L3,L4,C3,C4),
    verb_phrase(L4,L5,C4,C5).
question(L0,L4,C0,C4) :-
    verb_phrase(L0,L1,C0,C1),
    noun_phrase(L1,L2,C1,C2),
    noun_phrase(L2,L3,C2,C3),
    mp(L3,L4,C3,C4).
question(L0,L5,C0,C5) :-
    verb_phrase(L0,L1,C0,C1),
    noun_phrase(L1,L2,C1,C2),
    noun_phrase(L2,L3,C2,C3),
    noun_phrase(L3,L4,C3,C4),
    verb_phrase(L4,L5,C4,C5).
question(L0,L2,C0,C2) :-
    noun_phrase(L0,L1,C0,C1),
    mp(L1,L2,C1,C2).

% noun_phrase(L0,L1,C0,C1) is true if C0-C1 is the constraints from noun phrase L0-L1
noun_phrase(L0,L4,C0,C3) :-
    det(L0,L1,C0,C1),
    adjectives(L1,L2,C1,C2),
    noun(L2,L3),
    mp(L3,L4,C2,C3).
noun_phrase(L,L,C,C).

% verb_phrase(L0,L1,C0,C1) is true if C0-C1 is the constraints from verb phrase L0-L1
verb_phrase(L0,L4,C0,C3) :-
    verbs(L0,L1),
    adj(L1,L2,C0,C1),
    mp(L2,L3,C1,C2),
    mp(L3,L4,C2,C3).
verb_phrase(L0,L4,C0,C3) :-
    verb(L0,L1),
    noun_phrase(L1,L2,C0,C1),
    mp(L2,L3,C1,C2),
    mp(L3,L4,C2,C3).
verb_phrase([to | L0],L1,C0,C1) :-
    verb_phrase(L0,L1,C0,C1).

% A modifying phrase contains either a time or ingredient constraint
mp(L0,L3,C0,C1) :-
    p(L0,L1),
    max_constraint(L1,L2),
    time_constraint(L2,L3,C0,C1).
mp(L0,L4,C0,C3) :-
    p(L0,L1),
    det(L1,L2,C0,C1),
    ingredient(L2,L3,C1,C2),
    additional_ingredients(L3,L4,C2,C3).
mp(L0,L2,C0,C1) :-
    p(L0,L1),
    noun_phrase(L1,L2,C0,C1).
mp([that | L0],L2,C0,C2) :-
    noun_phrase(L0,L1,C0,C1),
    verb_phrase(L1,L2,C1,C2).
mp(L,L,C,C).

% Ingredient constraint
ingredient([Food | L],L,['&includeIngredients=',Food | C],C) :-
    food(Food).

additional_ingredients([and | L0],L1,C0,C1) :-
    additional_ingredients(L0,L1,C0,C1).
additional_ingredients([Food | L0],L1,[',',Food | C0],C1) :-
    food(Food),
    additional_ingredients(L0,L1,C0,C1).
additional_ingredients([Det | L0],L1,C0,C1) :-
    det(Det),
    additional_ingredients(L0,L1,C0,C1).
additional_ingredients(L,L,C,C).

% Time constraint
max_constraint([at,most | L],L).
max_constraint([under | L],L).
max_constraint([less,than | L],L).
max_constraint([no,more,than | L],L).
max_constraint(L,L).

time_constraint([Num,minutes | L],L,['&maxReadyTime=',Num | C],C) :-
    number(Num).

time_constraint([Num,hour | L],L,['&maxReadyTime=',Mins | C],C) :-
    number(Num),
    Mins is Num * 60.

time_constraint([Num,hours | L],L,['&maxReadyTime=',Mins | C],C) :-
    number(Num),
    Mins is Num * 60.

time_constraint([an, hour | L],L,['&maxReadyTime=',60 | C],C).
time_constraint([half, an, hour | L],L,['&maxReadyTime=',30 | C],C).

% Determiners provide no additional constraints
det([the | L],L,C,C).
det([a | L],L,C,C).
det([an | L],L,C,C).
det([some | L],L,C,C).
det(L,L,C,C).

det(the).
det(a).
det(an).
det(some).

% adjectives(L0,L2,C0,C2) is true if
% L0-L2 is a sequence of adjectives that imposes constraints C0-C2
adjectives(L0,L2,C0,C2) :-
    adj(L0,L1,C0,C1),
    adjectives(L1,L2,C1,C2).
adjectives(L,L,C,C).

% Cuisine constraint
adj([C1, C3 | L],L, ['&cuisine=',Cuisine|C],C) :-
    atom_concat(C1,'%20',C2),
    atom_concat(C2,C3,Cuisine),
    downcase_atom(Cuisine,LCuisine),
    cuisine(LCuisine).
adj([Cuisine | L],L, ['&cuisine=',Cuisine|C],C) :-
    downcase_atom(Cuisine,LCuisine),
    cuisine(LCuisine).

% Diet constraint
adj([D1, D3 | L],L, ['&diet=',Diet|C],C) :-
    atom_concat(D1,'%20',D2),
    atom_concat(D2,D3,Diet),
    downcase_atom(Diet,LDiet),
    diet(LDiet).
adj([Diet | L],L, ['&diet=',Diet|C],C) :-
    downcase_atom(Diet,LDiet),
    diet(LDiet).
adj([D1, '-', D3 | L],L, ['&diet=',Diet|C],C) :-
    atom_concat(D1,'-',D2),
    atom_concat(D2,D3,Diet),
    downcase_atom(Diet,LDiet),
    diet(LDiet).

adj([Word | L],L,C,C) :-
    s(_,_,Word,a,_,_).
adj([Word | L],L,C,C) :-
    dif(Word,chicken),
    s(_,_,Word,s,_,_).


% food(Word) is true if Word is an ingredient
food(Word) :-
    s(ID,_,Word,n,_,_),
    food_cat(HID),
    hyp(ID, HID).
food(Word) :-
    s(ID,_,Word,n,_,_),
    food_cat(HHID),
    hyp(ID, HID),
    hyp(HID, HHID).
food(PluralWord) :-
    atom_concat(Word,s,PluralWord),
    food(Word).
food(PluralWord) :-
    atom_concat(Word,es,PluralWord),
    food(Word).

noun([Word | L],L) :-
    s(_,_,Word,n,_,_).
noun([PluralWord | L],L) :-
    atom_concat(Word,s,PluralWord),
    s(_,_,Word,n,_,_).
noun([PluralWord | L],L) :-
    atom_concat(Word,es,PluralWord),
    s(_,_,Word,n,_,_).
noun([something | L],L).
noun([me | L],L).

verb([has | L],L).
verb([Word | L],L) :-
    s(_,_,Word,v,_,_).
verb([Word | L],L) :-
    atom_concat(W,s,Word),
    s(_,_,W,v,_,_).

verbs(L0,L2) :-
    verb(L0,L1),
    verbs(L1,L2).
verbs(L,L).

p([with | L],L).
p([in | L],L).
p([within | L],L).
p([using | L],L).
p([that | L0],L1) :-
    verb(L0,L1).

% Food categories
food_cat(107555863).  % food
food_cat(107566340).  % foodstuff
food_cat(100021265).  % nutrient
food_cat(107809096).  % ingredient
food_cat(113134947).  % fruit
food_cat(107707451).  % vegetable
food_cat(107858595).  % sweetener
food_cat(107809368).  % seasoning
food_cat(107672135).  % edible fat
food_cat(107737081).  % edible nut
food_cat(107705711).  % produce
food_cat(107707451).  % vegetable
food_cat(107649854).  % meat
food_cat(101789740).  % poultry
food_cat(107829412).  % sauce

% Supported cuisines
cuisine(african).
cuisine(american).
cuisine(british).
cuisine(cajun).
cuisine(caribbean).
cuisine(chinese).
cuisine('eastern%20european').
cuisine(european).
cuisine(french).
cuisine(german).
cuisine(greek).
cuisine(indian).
cuisine(irish).
cuisine(italian).
cuisine(japanese).
cuisine(jewish).
cuisine(korean).
cuisine('latin%20american').
cuisine(mediterranean).
cuisine(mexican).
cuisine('middle%20eastern').
cuisine(nordic).
cuisine(southern).
cuisine(spanish).
cuisine(thai).
cuisine(vietnamese).

% Supported diets
diet('gluten%20free').
diet(ketogenic).
diet(vegetarian).
diet('lacto-vegetarian').
diet('ovo-vegetarian').
diet(vegan).
diet(pescetarian).
diet(paleo).
diet(primal).
diet('whole30').


% for testing
getHyp(Word, H, HID) :-
    s(ID,_,Word,_,_,_),
    s(HID,_,H,_,_,_),
    hyp(ID, HID).
