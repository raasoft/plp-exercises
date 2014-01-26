% Consider the operation revmap which reverses a list and then performs a map: 
% i.e. revmap (*2) [1,2,3] is [6,4,1]
% Define a Prolog implementation of revmap traversing the list only once. 

rev(List,Answer) :- accRev(List, [], Answer).
        
accRev([X|XS], ACC, L) :- accRev(XS,[X|ACC], L).
accRev([],L,L).

% But now I have accRev and not revmap!

revMap(Function, List, Answer) :- accRevMap(Function, List, [], Answer).

accRevMap(F, [X|XS], ACC, L) :- call(F, X, W), accRev(XS,[W|ACC], L).
accRevMap(F, [],L,L).

%% test it
cube(X,R) :- R is X*X*X.
main :- revMap(cube, [2,3,4], X), print(X), nl.


%% Prof solution
%% revmap(F,[X|Y],Z,W) :- call(F,X,X1), revmap(F,Y,[X1|Z],W). 
%% revmap(_,[],X,X).