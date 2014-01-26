%% Consider binary trees represented as a hierarchic lists, where each node is a list
%% [node, subtree1, subtree2]. Leaves are just symbols. 

%% In the colored subtree problem, we take as input a tree, and put into each internal node 
%% a number representing the number of different leaves present in its subtrees.

%% E.g. given this tree: [R,[X,yellow,brown],[Y,blue,yellow]] the solution is: R = 3, X = Y = 2. 
%% Define the col_tree predicate, that solves the colored subtree problem.

%% Hint: the predicate union(X,Y,Z) holds if the list Z is the union of X and Y, seen as sets.





%% We need to pay attention: we must count ONLY DIFFERENT LEAVES!

len([], Z) :- Z is 0.
len([_|Y], Z) :- len(Y,N), Z is N+1. 

intersect([ ], _, [ ]).
intersect([X|R], Y, [X|Z]) :- member(X, Y), !, intersect(R, Y, Z).
intersect([_|R], Y, Z) :- intersect(R, Y, Z).

%% Casi base
col_tree([1, X, X], [X]) 	:- atomic(X).
col_tree([2, X, Y], [X, Y]) :- atomic(X), atomic(Y).

%% Caso alberi sbilanciati a destra
col_tree([N1, Tree1, Tree2]) :- 		atomic(Tree1),
										col_tree(Tree2, Col2),
										union([Tree1], Col2, Colors), 
										len(Colors, N1).

%% Caso alberi sbilanciati a sinistra
col_tree([N1, Tree1, Tree2]) :- 		col_tree(Tree1, Col1),
										atomic(Tree2),
										union(Col1, [Tree2], Colors), 
										len(Colors, N1).

%% Caso alberi bilanciati
col_tree([N1, Tree1, Tree2]) :- 		col_tree(Tree1, Col1),
										col_tree(Tree2, Col2),
										union(Col1, Col2, Colors), 
										len(Colors, N1).

main :- col_tree([R,[X,yellow,brown],[Y,blue,yellow]]), writef('Lollete %t %t %t\n', [R, X, Y]).
%% main :- intersect([yellow,brown],[blue,yellow], Z), writef('Lollete %t\n', [Z]).









