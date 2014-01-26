% ===========================
% Consider a tree whose nodes are stored as lists, and the first
% element is the value stored at that node, while the other elements are subtrees.
% Define a possibly concise function to find an element in the tree

% version 1:
% we suppose that trees are stored as lists [key, subtree_1, subtree_2, ...]
% e.g. [root,[a,[b,1,2,3],[c,2],[f,[g,x,y]]]]

findInList(_, []) :- fail.
findInList(Y, [Y|_]).
findInList(Y, [X|Rest]) :- findInList(Y,X) ; findInList(Y, Rest). % ; means OR

main :- findInList(2, [root,[a,[b,1,2,3],[c,2],[f,[g,x,y]]]]), findInList(a, [root,[a,[b,1,2,3],[c,2],[f,[g,x,y]]]]),  nl.

%% Prof solution
%% findlist([],_) :- fail.
%% findlist([X|_],X).
%% findlist([Y|L],X) :- findlist(Y,X) ; findlist(L,X). 

%% testfindlist :- 
%%     findlist([root,[a,[b,1,2,3],[c,2],[f,[g,x,y]]]], f).
