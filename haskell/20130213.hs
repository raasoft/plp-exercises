import Prelude

{-

CBCB Inc. produces its goods (bikes and other stuff) by assembling various parts coming from several suppliers. 
CBCB is connected to its suppliers through a number of supply brokers, and each of them is specialized in dealing one type of item 
(e.g. wheels, components, brakes...). 

At a fixed scheduled times (say, once every three months) each broker sends to CBCB's server a message containing the current offers 
from its suppliers (e.g. of wheels). Such message is a sequence of offers of analogous items. 
CBCB's server enqueues such sequence in a sequence of sequences; when this is complete, the servers calls the procedure allPossibleBikes 
to return all the possible combinations of bikes that could be built.

Example:

supply broker 1 sends (ultra-wheel-1, WheelyWheel);
supply broker 2 sends ("very nice frame", "another frame", "frame000"); 
supply broker 3 sends (3444,712,9938,115403).

In this case allPossibleBikes should return the sequence:

(ultra-wheel-1 "very nice frame" 3444) 
(WheelyWheel "very nice frame" 3444)
(ultra-wheel-1 "another frame" 3444) 
(WheelyWheel "another frame" 3444) 
(ultra-wheel-1 "frame000" 3444) 
(WheelyWheel "frame000" 3444) 
(ultra-wheel-1 "very nice frame" 712) 
(WheelyWheel "very nice frame" 712) 
(ultra-wheel-1 "another frame" 712) 
(WheelyWheel "another frame" 712) 
(ultra-wheel-1 "frame000" 712) 
(WheelyWheel "frame000" 712) 
(ultra-wheel-1 "very nice frame" 9938) 
(WheelyWheel "very nice frame" 9938) 
(ultra-wheel-1 "another frame" 9938) 
(WheelyWheel "another frame" 9938) 
(ultra-wheel-1 "frame000" 9938) 
(WheelyWheel "frame000" 9938)
(ultra-wheel-1 "very nice frame" 115403) 
(WheelyWheel "very nice frame" 115403) 
(ultra-wheel-1 "another frame" 115403) 
(WheelyWheel "another frame" 115403) 
(ultra-wheel-1 "frame000" 115403) 
(WheelyWheel "frame000" 115403)

1) Define a suitable data structure for CBCB's orders in Haskell, knowing that the sequence can contain any number of elements, 
and that each supply broker message is a nonempty sequence of any number of offers. 
For simplicity, assume that each offer may either be represented as a natural number, or a string.

2) Define a purely functional version of allPossibleBikes in Haskell.

  ghci

  Main*> :l 20130213.hs
  Main*> 

-}

data Offer = Int | Char | String deriving Show
type Orders = [[Offer]]

allPossibleBikes list = foldr k [[]] list where k m mx = [(x:xs) | x <- m, xs <- mx]





