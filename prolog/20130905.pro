%% Define a mixUp predicate, which takes an arithmetic expression without variables 
%% containing only the * and + operators, and puts in its second argument the value 
%% of the expression obtained by changing all the * operators to + and vice versa, 
%% but keeping the same syntactic structure.

%% E.g. mixUp(3*2+4,X) must return X = 20.
%% Note: use cut to avoid unnecessary backtracks.

mixUp(X1, X) :- atomic(X1), X is X1.
mixUp(X1 * X2, X) :- mixUp(X1, Q), mixUp(X2, W), X is Q + W.
mixUp(X1 + X2, X) :- mixUp(X1, Q), mixUp(X2, W), X is Q * W.

main :- mixUp(3*2+4,X), writef('Result %t\n', [X]).









