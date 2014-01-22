%% Define the operation revmap which reverses a list and then performs a map:

revMap(F,[X|XS], ACC, L) :- call(F, X, X1), revMap(F,XS,[X|ACC], L).
revMap(_,[],L,L).


%% Here call performs a query in a program