%% CBCB has been notified that some of its suppliers had problems with their servers, 
%% so they could include in the same sequence two or more copies of the same item.

%% 1. Define a procedure to get only unique bikes in the sequence obtained by 
%% allPossibleBikes of Ex 1.

%% 2. CBCB decided to filter the offer sequences from the supply brokers, 
%% to check which of them have problems. Write a procedure that, given an input sequence, 
%% returns only the repeated items in it.

        myLength([], Z) :- Z is 0.
        myLength([_|Y], Z) :- myLength(Y,N), Z is N+1. 

        mySubset([], []).
        mySubset([E|Tail], [E|NTail]) :- mySubset(Tail, NTail).
        mySubset([_|Tail], NTail)     :- mySubset(Tail, NTail).   

        predicate(X) :- myLength(X, N), N is 10.

findSubsetThatSatisfies(List, Predicate) :-  mySubset(List, X), call(Predicate, X).



main :- findSubsetThatSatisfies([1,2,3,4,1], predicate).


%% Prof solution