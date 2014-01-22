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

union([], L, L).
union([X|L], Y, [X|Z]) 	:-	union(L,Y,Z).

remdupl([],[]).
remdupl([X|Xs],[X|Ys]) :- \+ (member(X,Xs)), remdupl(Xs,Ys). 
remdupl([X|Xs],Out) :- member(X,Xs), remdupl(Xs,Out). 

%% intersect([],[],[]).
%% intersect(X,[],[]).
%% intersect([],X,[]).
%% intersect([X|Xs],[X|Ys], Z) :- (member(X,Xs), intersect(Xs, Ys).

col_tree([0, [], []]).
col_tree([1, X, []]) :- atomic(X).
col_tree([1, [], X]) :- atomic(X).
col_tree([2, X, Y]) :- atomic(X), atomic(Y).
col_tree([N1, [NL, LL], [NR, LR]]) :- 	N1 is NL + NR, 
									union(LL, LR, Z), 
									intersection(LL, Z, W1),
									len(W1, NL),
									intersection(LR, Z, W2),
									len(W2, NR)
									.

%% col_tree(N, [R, L11])

main :- col_tree([R,[X,yellow,brown],[Y,blue,yellow]]), writef('Lollete %t\n', [R, X, Y]).