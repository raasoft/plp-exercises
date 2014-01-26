%% Define the prefix predicate that holds iff its second argument 
%% is a prefix of the first argument. 

%% E.g. prefix("Hello world", "Hello") is true, while prefix("Hello world", "wor") is not.

%% Define an analogous predicate for suffixes.
%% E.g. suffix("Hello world", "world") is true, while suffix("Hello world", "Hello") is not.

%% Define the infix predicate that holds iff its second argument is a substring 
%% of the first argument. (Hint: an infix is a prefix of a suffix.)

%% Define the overlap predicate that holds iff its two argument strings actually overlap, 
%% i.e. either one is an infix of the other, or one’s prefix is a suffix of the other.

prefix(_, []).
prefix([X|Rest1], [X|Rest2]) :- prefix(Rest1, Rest2).

suffix(_, []).
suffix(List1, List2) :- reverse(List1, TsiL1), reverse( List2, TsiL2), prefix(TsiL1, TsiL2).

infix([], _) :- fail.
infix(Rest1, Rest2) :- suffix(Rest1, X), prefix(X, Rest2), !.

overlap(Rest1, Rest2) :- !, infix(Rest1, Rest2).
overlap(Rest1, Rest2) :- !, infix(Rest2, Rest1).
overlap(Rest1, Rest2) :- !, prefix(Rest1, X), suffix(Rest2, X).
overlap(Rest1, Rest2) :- !, prefix(Rest2, X), suffix(Rest1, X).

 main :- overlap([1,2], [2,3]),  nl.

%% Prof solution
%% prefix([X|_], [X]).
%% prefix([X|Xs], [X|Z]) :- prefix(Xs, Z).

%% suffix(X,X) :- \+ X = [].
%% suffix([_|Xs], S) :- suffix(Xs, S).

%% infix(X,Y) :- suffix(X, SuffX), prefix(SuffX, Y).

%% overlaph(X,Y) :- suffix(X, SuffX), prefix(Y, SuffX).
%% overlap(X,Y) :- overlaph(X, Y); overlaph(Y, X);
%%                 infix(X, Y); infix(Y, X).
