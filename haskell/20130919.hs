import Prelude

{-

  Define the function infixes, which takes a list g as input and returns 
  the list of all infixes (i.e. non-empty contiguous sublists) of g.

  For instance, infixes "ciao" is the list ["o","ao","iao","ciao","a","ia","cia","i","ci","c"] 
  (remember that a string is a list of characters in Haskell).

  ghci

  Main*> :l 20130919.hs
  Main*> infixes "ciao"
  ["o","ao","iao","ciao","a","ia","cia","i","ci","c"] 

-}

tails g = take (length g) (iterate mytail g)
mytail :: [a] -> [a]
mytail [x]         =  [x]  
mytail (x:xs)      =  xs
mytail []          =  []

inits g = take (length g) (iterate myinit g)
myinit :: [a] -> [a] --init [1,2,3,4,5] = [1,2,3,4]
myinit [x]         =  []  
myinit (x:xs)      =  x : init xs  
myinit []          =  []

myIsPrefix subStr str = if filter (==subStr) (inits str) == [] then False else True
myIsInfix subStr str = if filter (myIsPrefix subStr) (tails str) == [] then False else True

subsequences :: [a] -> [[a]]
subsequences xs         =  [] : nonEmptySubsequences xs

nonEmptySubsequences         :: [a] -> [[a]]
nonEmptySubsequences []      =  []
nonEmptySubsequences (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences xs)
								where f ys r = ys : (x : ys) : r

infixes g = filter (\x -> myIsInfix x g) (subsequences g)