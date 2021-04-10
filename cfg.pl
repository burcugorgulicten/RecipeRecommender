:- discontiguous s/6, s/4.  % suppress warnings
:- ensure_loaded(['wordnet/wn_s', 'wordnet/wn_hyp']).

% question(Q,L,C0,C1) is true if C0-C1 is the constraints from question Q-L
question(['What',is | L0],L1,C0,C1) :-
    noun_phrase(L0,L1,C0,C1).
question(['What' | L0],L4,C0,C4) :-
    noun_phrase(L0,L1,C0,C1),
    verb_phrase(L1,L2,C1,C2),
    noun_phrase(L2,L3,C2,C3),
    verb_phrase(L3,L4,C3,C4).

% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional modifying phrase:
noun_phrase(L0,L4,C0,C3) :-
    det(L0,L1,C0,C1),
    adjectives(L1,L2,C1,C2),
    noun(L2,L3),
    mp(L3,L4,C2,C3).
noun_phrase(L,L,C,C).

% a verb phrase is a verb followed by a noun phrase and one or two optional mps
verb_phrase(L0,L4,C0,C3) :-
    verb(L0,L1),
    noun_phrase(L1,L2,C0,C1),
    mp(L2,L3,C1,C2),
    mp(L3,L4,C2,C3).

% a modifying phrase contains either a time or ingredient constraint
mp(L,L,C,C).
mp(L0,L3,C0,C1) :-
    p(L0,L1),
    max_constraint(L1,L2),
    time_constraint(L2,L3,C0,C1).
mp(L0,L2,C0,C1) :-
    p(L0,L1),
    ingredient(L1,L2,C0,C1).

ingredient([Food | L],L,['&includeIngredients=',Food | C],C) :-
    food(Food).

max_constraint([at,most | L],L).
max_constraint([under | L],L).
max_constraint([less,than | L],L).
max_constraint(L,L).

time_constraint([Num,minutes | L],L,['&maxReadyTime=',Num | C],C) :-
    number(Num).

det([the | L],L,C,C).
det([a | L],L,C,C).
det(L,L,C,C).

% adjectives(L0,L2,C0,C2) is true if
% L0-L2 is a sequence of adjectives that imposes constraints C0-C2
adjectives(L0,L2,C0,C2) :-
    adj(L0,L1,C0,C1),
    adjectives(L1,L2,C1,C2).
adjectives(L,L,C,C).

adj([C1, C3 | L],L, ['&cuisine=',Cuisine|C],C) :-
    atom_concat(C1,' ',C2),
    atom_concat(C2,C3,Cuisine),
    cuisine(Cuisine).
adj([Cuisine | L],L, ['&cuisine=',Cuisine|C],C) :-
    cuisine(Cuisine).

adj([D1, D3 | L],L, ['&diet=',Diet|C],C) :-
    atom_concat(D1,' ',D2),
    atom_concat(D2,D3,Diet),
    diet(Diet).
adj([Diet | L],L, ['&diet=',Diet|C],C) :-
    diet(Diet).


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

% adj([Word | L],L) :-
%     s(_,_,Word,a,_,_).

noun([Word | L],L) :-
    s(_,_,Word,n,_,_).

verb([Word | L],L) :-
    s(_,_,Word,v,_,_).

p([with | L],L).
p([in | L],L).
p([within | L],L).
p([using | L],L).

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
cuisine('African').
cuisine('American').
cuisine('British').
cuisine('Cajun').
cuisine('Caribbean').
cuisine('Chinese').
cuisine('Eastern European').
cuisine('European').
cuisine('French').
cuisine('German').
cuisine('Greek').
cuisine('Indian').
cuisine('Irish').
cuisine('Italian').
cuisine('Japanese').
cuisine('Jewish').
cuisine('Korean').
cuisine('Latin American').
cuisine('Mediterranean').
cuisine('Mexican').
cuisine('Middle Eastern').
cuisine('Nordic').
cuisine('Southern').
cuisine('Spanish').
cuisine('Thai').
cuisine('Vietnamese').

% Supported diets
diet('Gluten free').
diet('Ketogenic').
diet('Vegetarian').
diet('Lacto-Vegetarian').
diet('Ovo-Vegetarian').
diet('Vegan').
diet('Pescetarian').
diet('Paleo').
diet('Primal').
diet('Whole30').


% for testing
getHyp(Word, H, HID) :-
    s(ID,_,Word,_,_,_),
    s(HID,_,H,_,_,_),
    hyp(ID, HID).
