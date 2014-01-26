%% Define a Prolog predicate subsetsum that checks if there exists a sublist of the 
%% first argument such that the sum of all its elements is equal to the second argument.

%% E.g.: subsetsum([1,2,3,4],19) is false, while subsetsum([1,2,3,4],7) is true.

subset([], []).
subset([E|Tail], [E|NTail]) :- 	subset(Tail, NTail).
subset([_|Tail], NTail) 	:-	subset(Tail, NTail).

sumall([N], N) :- number(N).
sumall([X|Rest], N) :- number(X), sumall(Rest, Y), N is X + Y.

subsetsum(List, Sum) :-  subset(List, X), sumall(X, Sum).

main :- subsetsum([1,2,3,4], 2).

%% Prof solution
