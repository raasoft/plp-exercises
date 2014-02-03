import Prelude

{-

  Consider the parent-pointer implementation of general trees (PPT), where every node has local data (e.g. a name), and only one pointer, 
  pointing to its parent. Of course, roots nodes do not point to other nodes.
  
  PPTs are generally implemented as arrays containing pairs (index of parent, node name), 
  and one of such data structures can contain one or more different trees, e.g. using a Scheme-like syntax:
   
      #((? . R) (0 . A) (0 . B) (1 . C) (1 . D) (1 . E) (2 . F) (? . W) (7 . X) (7 . Y))

  where root nodes have first component “?”. 
  Nodes are usually referenced through their index.
  
  PPTs are efficient for checking if two nodes belong to the same tree (we have just to check if the root is
  the same for both), so are often used to represent partitions.

#### Define the data structure, findRoot and union operations for immutable PPTs in Haskell.

#### Remainder and hints: immutable arrays are offered by the module Data.Array; 
#### an array with index type Int and containing elements of type Type has type Array Int Type. 
#### You can use Int for indexes.
#### Typical operations on arrays are ! to access an element (e.g. A ! 3 is equivalent to A[3] in C), 
#### and // for updates (e.g. A // [(3, 12)] creates a new array containing the same elements of A but for A[3] = 12).

-}

import Data.Array
-- A ! 3 == A[3] 
-- A // [(3,12)]  create a NEW array updating the index 3 with 12


-- Ogni nodo dell'albero é o un nodo Parent che prende come argomento l'intero (la posizione) oppure é nodo di Root
data Node = Parent Int | Root deriving (Show, Eq)

-- Definisco che un ParentTree é un array fatto da coppie ("Tipologia Nodo", "valore")
type ParentTree a = Array Int (Node, a)


parent :: ParentTree a -> Int -> Node
parent tree nodeIndex = fst(tree ! nodeIndex) -- prendo il primo termine 

findRoot :: ParentTree a -> Int
findRoot tree = findRootAux tree 0

findRootAux :: ParentTree a -> Int -> Int
findRootAux tree nodeIndex =  case parent tree nodeIndex of
                              Root -> nodeIndex
                              (Parent parentIndex) -> findRootAux tree parentIndex


setParent :: ParentTree a -> Int -> Int -> ParentTree a
setParent tree nodeIndex nodeParentIndex = tree // [(nodeIndex, (Parent nodeParentIndex, snd $ tree // nodeIndex) )]

union :: ParentTree a -> ParentTree a -> ParentTree a
union treeA treeB = let rootA = findRoot treeA
                        rootB = findRoot treeB
                    in setParent rootA rootB -- this returns the tree
