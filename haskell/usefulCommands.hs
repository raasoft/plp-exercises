-------------------------------------------------------------------------- 
-- STANDARD TYPE CLASSES
-- FUNCTIONS ON PAIRS
-- FUNCTIONS ON LISTS
-- CUSTOM TYPE CLASSES
-- MONADIC TYPES
-- CUSTOM DATA TYPE
-- NUMERICAL FUNCTIONS
-- MONADIC FUNCTIONS
-- FUNCTIONS ON FUNCTIONS
-- FUNCTIONS ON BOOLS
-- FUNCTIONS ON MAYBE
-- FUNCTIONS ON CHAR
-- PARALLELISM
-------------------------------------------------------------------------- 


-------------------------------------------------------------------------- 
-- SIMPLE DATA TYPES
-------------------------------------------------------------------------- 

-- ### Lists

    -- Sequence of OMOGENEOUS elements
     
    [1,2,3]         :: [Int]
    ['a', 'b', 'c'] :: [Char]
    "abc"           :: [Char] -- N.B. "abc" is sugar for ['a', 'b', 'c'] --

-- ### Enumerations
    
    -- Short way of define lists

    [1..10]             -- List of numbers 
    [100..]             -- Infinite list of numbers 
    [110..100]          -- Empty list
    [110, 109 .. 100]   -- Backward list from 110 to 100 
    [-1,3..99]          -- List from -1 to 99 by 4. 
    ['a' .. 'z']        -- List from a to z

    -- List and tuples

    []                   -- Empty list (a.k.a. nil) 
    1 : 2 : 3 : []       -- same as [1,2,3] (cons operator)
    (1,"a")              -- 2-element TUPLE of a number and a string
    (head, tail, 3, 'a') -- 4-element tuple of two functions, a number and a character.





--------------------------------------------------------------------------
-- FUNCTIONS
-------------------------------------------------------------------------- 

-- ### Pattern Matching 

    -- Matches when the string "y" is given.
    agree1 "y" = "Great!"
    -- Matches when the string "n" is given.
    agree1 "n" = "Too bad."
    -- Matches when string beginning
    -- with ’y’ given.
    agree1 (’y’:_) = "YAHOO!"
    -- Matches for any other value given.
    agree1 _ = "SO SAD."
    -- match a list called ls and describe its composition using @
    len ls@(l:tail) = "List starts with " ++
                    show l ++ 
                    " its tail is " ++
                    show tail ++ 
                    " and is " ++
                    show (length ls) ++ " items long."
    -- Guards (boolean functions) usage
    what [] = "empty string!"
    what (c:_)
    | isUpper c = "upper case!"
    | isLower c = "lower case"
    | otherwise = "not a letter!"

    -- Use pattern-matching to
    -- get first character
    sentenceCase (s:rest) =
    if isLower s
    then toUpper s : rest
    else s : rest



-- Lambdas

  multBy n = \m -> n * m -- returns a lambda that multiplies two numbers

  isMixedCase str =      -- all :: (a -> Bool) -> [a] -> Bool 
    all (\c -> isSpace c ||
    isLower c ||
    isUpper c) str



--------------------------------------------------------------------------
-- FUNCTIONS ON LISTS OR ENUMERATIONS
-------------------------------------------------------------------------- 

-- ### Common

  {-
    map f xs is the list obtained by applying f to each element of xs, i.e.,
  -}
  map :: (a -> b) -> [a] -> [b] 
    map f xs = [ f x | x <- xs ]

    {-
      map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]
      map f [x1, x2, ...] == [f x1, f x2, ...]
    -}

  {-
    revmap f xs is the list obtained by applying f to each element of reverse xs, i.e.,
    It traverses the list more than once!
  -}
  revMap :: (a -> a) -> [a] -> [a]
    revMap f xs = map f (reverse xs)

  {-
    Append two lists.
    If the first list is not finite, the result is the first list.
  -}
  (++) :: [a] -> [a] -> [a]
    xs ++ ys = foldr (:) ys xs

    {-
      [x1, ..., xm] ++ [y1, ..., yn] = [x1, ..., xm, y1, ..., yn]
      [x1, ..., xm] ++ [y1, ...] = [x1, ..., xm, y1, ...]
    -}

  {-
    filter, applied to a predicate and a list, returns the list of those elements that satisfy the predicate;
  -}
  filter :: (a -> Bool) -> [a] -> [a]
    filter p xs = [ x | x <- xs, p x]

  {-
    Extract the first element of a list, which must be non-empty.
  -}
  head :: [a] -> a
    head [x] = x
    head (x:xs) = x

  {-
    Extract the last element of a list, which must be finite and non-empty.
  -}
  last :: [a] -> a
    last [] = []
    last [x] = x
    last (x:xs) = last xs

  {-
    Extract the elements after the head of a list, which must be non-empty.
  -}
  tail :: [a] -> [a]
    tail (x:xs) = xs

  {-
    Return all the elements of a list except the last one. The list must be non-empty.
  -}
  init :: [a] -> [a]

  {-
    Test whether a list is empty.
  -}
  null :: [a] -> Bool


  {-
    O(n). length returns the length of a finite list as an Int. 
    It is an instance of the more general genericLength, the result type of which may be any kind of number.
  -}
  length :: [a] -> Int


  {-
    List index (subscript) operator, starting from 0. 
    It is an instance of the more general genericIndex, which takes an index of any integral type.
  -}
  (!!) :: [a] -> Int -> a
    (x:_)  !! 0 = x
    (_:xs) !! n = xs !! (n-1)

  {-
    reverse xs returns the elements of xs in reverse order. xs must be finite.
  -}
  reverse :: [a] -> [a]
    reverse = foldl (flip (:)) []


-- ### Reducing lists

{-
  foldl, applied to a binary operator, a starting value (typically the left-identity of the operator), and a list, 
  reduces the list using the binary operator, from left to right. The list must be finite.
-}
foldl :: (a -> b -> a) -> a -> [b] -> aSource
  foldl f z [x1, x2, ..., xn] = (...((z `f` x1) `f` x2) `f`...) `f` xn


{- 
  foldl1 is a variant of foldl that has no starting value argument, and thus must be applied to non-empty lists. 
-}
foldl1 :: (a -> a -> a) -> [a] -> aSource

{- 
  foldr, applied to a binary operator, a starting value (typically the right-identity of the operator), and a list, reduces the list using the binary operator, from right to left: 
-}
foldr :: (a -> b -> b) -> b -> [a] -> bSource
  foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)

{-
  foldr1 is a variant of foldr that has no starting value argument, and thus must be applied to non-empty lists.
-}
foldr1 :: (a -> a -> a) -> [a] -> aSource


-- ### Special folds

  {-
    and returns the conjunction of a Boolean list. For the result to be True, the list must be finite; 
    False, however, results from a False value at a finite index of a finite or infinite list.
  -}
  and :: [Bool] -> BoolSource


  {- 
    or returns the disjunction of a Boolean list. For the result to be False, the list must be finite; 
    True, however, results from a True value at a finite index of a finite or infinite list.
  -}
  or :: [Bool] -> BoolSource

  {-
    Applied to a predicate and a list, any determines if any element of the list satisfies the predicate. 
    For the result to be False, the list must be finite; 
    True, however, results from a True value for the predicate applied to an element at a finite index of a finite or infinite list.
  -}
  any :: (a -> Bool) -> [a] -> BoolSource

  {-
    Applied to a predicate and a list, all determines if all elements of the list satisfy the predicate. 
    For the result to be True, the list must be finite; 
    False, however, results from a False value for the predicate applied to an element at a finite index of a finite or infinite list.
  -}
  all :: (a -> Bool) -> [a] -> BoolSource

  {- 
    The sum function computes the sum of a finite list of numbers. 
  -}
  sum :: Num a => [a] -> aSource

  {- 
    The product function computes the product of a finite list of numbers.
  -}
  product :: Num a => [a] -> aSource

  {-
    Concatenate a list of lists.
  -}
  concat :: [[a]] -> [a]Source

  {-
    Map a function over a list and concatenate the results.
  -}
  concatMap :: (a -> [b]) -> [a] -> [b]Source

  {-
    maximum returns the maximum value from a list, which must be non-empty, finite, and of an ordered type. 
    It is a special case of maximumBy, which allows the programmer to supply their own comparison function.
  -}
  maximum :: Ord a => [a] -> aSource

  {-
    minimum returns the minimum value from a list, which must be non-empty, finite, and of an ordered type. 
    It is a special case of minimumBy, which allows the programmer to supply their own comparison function. 
  -}
  minimum :: Ord a => [a] -> aSource

asdasd = 2


-- ### Sublists

  {-
    take n, applied to a list xs, returns the prefix of xs of length n, or xs itself if n > length xs.
  -}
  take :: Int -> [a] -> [a]
  
  take n (x:xs) =
     | n == length (x:xs) = (x:xs)
     | otherwise = x ++ take (n-1) xs

    {-
      take 5 "Hello World!" = "Hello"
      take 3 [1,2,3,4,5] = [1,2,3]
      take 3 [1,2] = [1,2]
      take 3 [] = []
      take (-1) [1,2] = []
      take 0 [1,2] = []
    -}

  {-
    drop n xs returns the suffix of xs after the first n elements, or [] if n > length xs.
  -}
  drop :: Int -> [a] -> [a]
  drop n xs | n <= 0 = xs
  drop _ [] = []
  drop n (_:xs) = drop (n-1) xs


    {-
      drop 6 "Hello World!" == "World!"
      drop 3 [1,2,3,4,5] == [4,5]
      drop 3 [1,2] == []
      drop 3 [] == []
      drop (-1) [1,2] == [1,2]
      drop 0 [1,2] == [1,2]
    -}

  {-
    splitAt n xs returns a tuple where first element is xs prefix of length n and second element is the remainder of the list.
    It is equivalent to (take n xs, drop n xs) when n is not _|_ (splitAt _|_ xs = _|_). 
    splitAt is an instance of the more general genericSplitAt, in which n may be of any integral type.
  -}
  splitAt :: Int -> [a] -> ([a], [a])
    
    {- 
      splitAt 6 "Hello World!" == ("Hello ","World!")
      splitAt 3 [1,2,3,4,5] == ([1,2,3],[4,5])
      splitAt 1 [1,2,3] == ([1],[2,3])
      splitAt 3 [1,2,3] == ([1,2,3],[])
      splitAt 4 [1,2,3] == ([1,2,3],[])
      splitAt 0 [1,2,3] == ([],[1,2,3])
      splitAt (-1) [1,2,3] == ([],[1,2,3])
    -}

  {-
    splitAt n xs returns a tuple where first element is xs prefix of length len xs/2 and second element is the remainder of the list.
  -}
  splitInHalves :: [a] -> ([a],[a])
  splitInHalves xs = splitAt (length xs `div` 2) xs 
    {-
      splitInHalves [1,2,3] = ([1,2], [3])
    -}


  {-
    takeWhile, applied to a predicate p and a list xs, returns the longest prefix 
    (possibly empty) of xs of elements that satisfy p
  -}
  takeWhile :: (a -> Bool) -> [a] -> [a]
    
    takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
    takeWhile (< 9) [1,2,3] == [1,2,3]
    takeWhile (< 0) [1,2,3] == []

  {-
    dropWhile p xs returns the suffix remaining after takeWhile p xs
  -}
  dropWhile :: (a -> Bool) -> [a] -> [a]Source

    dropWhile (< 3) [1,2,3,4,5,1,2,3] = [3,4,5,1,2,3]
    dropWhile (< 9) [1,2,3] = []
    dropWhile (< 0) [1,2,3] = [1,2,3]


  {-
    span, applied to a predicate p and a list xs, returns a tuple where first element is longest 
    prefix (possibly empty) of xs of elements that satisfy p and second element is the remainder of the list:
    span p xs is equivalent to (takeWhile p xs, dropWhile p xs)
  -}
  span :: (a -> Bool) -> [a] -> ([a], [a])

    span (< 3) [1,2,3,4,1,2,3,4] = ([1,2],[3,4,1,2,3,4])
    span (< 9) [1,2,3] = ([1,2,3],[])
    span (< 0) [1,2,3] = ([],[1,2,3])

  {-
    break, applied to a predicate p and a list xs, returns a tuple where first element is longest prefix 
    (possibly empty) of xs of elements that do not satisfy p and second element is the remainder of the list.
    break p is equivalent to span (not . p). 
  -}
  break :: (a -> Bool) -> [a] -> ([a], [a])Source

   break (> 3) [1,2,3,4,1,2,3,4] = ([1,2,3],[4,1,2,3,4])
   break (< 9) [1,2,3] = ([],[1,2,3])
   break (> 9) [1,2,3] = ([1,2,3],[])

  
{- 
  iterate f x returns an infinite list of repeated applications of f to x:
-}
iterate :: (a -> a) -> a -> [a]
  
  iterate f x = [x, f x, f (f x), ...]

{-
  repeat x is an infinite list, with x the value of every element. 
-}
repeat :: a -> [a]

{- 
  replicate n x is a list of length n with x the value of every element. It is an instance of the more general genericReplicate, in which n may be of any integral type. 
-}
replicate :: Int -> a -> [a]Source

{-
  cycle ties a finite list into a circular one, or equivalently, the infinite repetition of the original list. It is the identity on infinite lists. 
-}
cycle :: [a] -> [a]Source














                                      revMap :: (a -> a) -> [a] -> [a]
                                      revMap f (x:[]) = [f x]
                                      revMap f (x:xs) = ((revMap f xs)++[f x])

                                      my_map :: (a->b) -> [a] -> [b] -- Custom implementation of map
                                      my_map _ [] = []
                                      my_map f (x:xs) = f x : my_map f xs



                                      filter :: (a -> Bool) -> [a] -> [a] 
                                      filter p xs = [ x | x <- xs, p x ]

                                      -- ********* filter implementation **********
                                      -- filter(p, [e1, e2, ..., en]) = [ei | 1<=n<=n, p(ei)]
                                      my_filter :: (a -> Bool) -> [a] -> [a]
                                      my_filter _ [] = []
                                      my_filter p (x:xs)  | p x = x : my_filter p xs
                                                | not (p x) = my_filter p xs


                                      concat :: [[a]] -> [a]
                                      concat xss = foldr (++) [] xss

                                      concatMap :: (a -> [b]) -> [a] -> [b]
                                      concatMap f = concat . map f

                                      head, last :: [a] -> a
                                      head (x:_) = x

                                      last [x] = x
                                      last (_:xs) = last xs

                                      tail, init :: [a] -> [a]
                                      tail (_:xs) = xs
                                      -- tail [1,2,3,4,5] == [2, 3, 4, 5]

                                      init [x] = []
                                      init (x:xs) = x : init xs
                                      --init [1,2,3,4,5] = [1,2,3,4]

                                      null :: [a] -> Bool
                                      null [] = True
                                      null (_:_) = False


                                      length :: [a] -> Int
                                      length [] = 0
                                      length (_:l) = 1 + length l

                                      foldr :: (a -> b -> b) -> b -> [a] -> b
                                      foldr f z [] = z
                                      foldr f z (x:xs) = f x (foldr f z xs)

                                      foldl :: (a -> b -> a) -> a -> [b] -> a 
                                      foldl f z [] = z
                                      foldl f z (x:xs) = foldl f (f z x) xs

                                      -- ********* fold-right implementation **********
                                      -- fold-right(op, e, [e1, e2, ..., en]) = (e1 op ( e2 op ... (en op e)))
                                      my_fold_right :: (a -> b -> b) -> b -> [a] -> b
                                      my_fold_right op x [y] = op y x
                                      my_fold_right op x (y:ys) = op y (my_fold_right op x ys)

                                      -- ********* fold-left implementation **********
                                      -- fold-left(op, e, [e1, e2, ..., en]) = (((e op e1) op e2) op ... en)
                                      my_fold_left :: (a -> b -> a) -> a -> [b] -> a
                                      my_fold_left op x [y] = op x y
                                      my_fold_left op x (y:ys) = my_fold_left op (op x y) ys

                                      -- iterate f x returns an infinite list of repeated applications of f to x:
                                      -- iterate f x == [x, f x, f (f x), ...]
                                      iterate :: a -> [a]
                                      iterate f x = x : iterate f (f x)

                                      repeat :: a -> [a]
                                      repeat x = xs where xs = x:xs

                                      replicate :: Int -> a -> [a]
                                      replicate n x = take n (repeat x)

                                      -- rende una lista finita una lista circolare
                                      cycle :: [a] -> [a]
                                      cycle [] = error "Prelude.cycle: empty list"
                                      cycle xs = xss where xss = xs ++ xss

                                      take, drop :: Int -> [a] -> [a]
                                      take n _ | n <= 0 = []
                                      take _ [] = []
                                      take n (x:xs) = x : take (n-1) xs

                                      splitAt :: Int -> [a] -> ([a], [a])
                                      splitAt n xs = (take n xs, drop n xs)


                                      --  takeWhile, applied to a predicate p and a list xs, returns the 
                                      -- longest prefix (possibly empty) of xs of elements that satisfy p
                                      takeWhile, dropWhile :: (a -> Bool) -> [a] -> [a]
                                      takeWhile p [] = []
                                      takeWhile p (x:xs)  | p x = x : takeWhile p xs
                                                          | otherwise = []
                                      -- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]


                                      -- dropWhile p xs returns the suffix remaining after takeWhile p xs
                                      dropWhile p [] = []
                                      dropWhile p xs@(x:xss)  | p x = dropWhile p xss
                                                              | otherwise = xs
                                      --  dropWhile (< 3) [1,2,3,4,5,1,2,3] == [3,4,5,1,2,3]

                                                              
                                      lines, words :: String -> [String]
                                      -- lines "apa\nbepa\ncepa\n" == ["apa","bepa","cepa"]
                                      -- words "apa bepa\n cepa" == ["apa","bepa","cepa"]

                                      unlines, unwords :: [String] -> String
                                      -- the opposite of lines words

                                      and, or :: [Bool] -> Bool
                                      and = foldr (&&) True
                                      or = foldr (||) False


                                      --Applied to a predicate and a list, all determines if all elements of the 
                                      -- list satisfy the predicate. For the result to be True, the list must 
                                      -- be finite; False, however, results from a False value for the predicate 
                                      -- applied to an element at a finite index of a finite or infinite list.
                                      any,all :: (a -> Bool) -> [a] -> Bool
                                      any p = or . map p
                                      all p = and . map p

                                      elem, notElem :: (Eq a) => a -> [a] -> Bool
                                      elem x = any (== x)
                                      notElem x = all (/= x)

                                      lookup :: (Eq a) => a -> [(a,b)] -> Maybe b
                                      lookup key [] = Nothing
                                      lookup key ((x,y):xys)  | key == x = Just y
                                                              | otherwise = lookup key xys
                                                              
                                      sum, product :: (Num a) => [a] -> a
                                      sum = foldl (+) 0
                                      product = foldl (*) 1

                                      maximum, minimum :: (Ord a) => [a] -> a
                                      maximum [] = error "Prelude.maximum: empty list"
                                      maximum xs = foldl1 max xs

                                      minimum [] = error "Prelude.minimum: empty list
                                      minimum xs = foldl1 min xs


                                      zip :: [a] -> [b] -> [(a,b)]
                                      zip = zipWith (,)

                                      zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
                                      zipWith z (a:as) (b:bs) = z a b : zipWith z as bs
                                      zipWith _ _ _ = []

                                      ---- | The 'unzip' function takes a list of quadruples and returns two lists
                                      unzip :: [(a,b)] -> ([a],[b])
                                      unzip = foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])


                                      --This function performs purely a cartesian product of the list passed to it: [[1,2,3],[4,5,6],[7,8,9]]
                                      cartesianProductNWays ms = foldr k [[]] ms where k m mx = [(x:xs) | x <- m, xs <- mx]
                                      -- result: 
                                      -- [[1,4,7],[1,4,8],[1,4,9],[1,5,7],[1,5,8],[1,5,9],[1,6,7],[1,6,8],[1,6,9],
                                      --  [2,4,7],[2,4,8],[2,4,9],[2,5,7],[2,5,8],[2,5,9],[2,6,7],[2,6,8],[2,6,9],
                                      --  [3,4,7],[3,4,8],[3,4,9],[3,5,7],[3,5,8],[3,5,9],[3,6,7],[3,6,8],[3,6,9]]
                                      -- to obtain the same result without purely functions, please use sequence [[1,2,3],[4,5,6],[7,8,9]]


                                      -- The 'nub' function removes duplicate elements from a list.
                                      -- In particular, it keeps only the first occurrence of each element.
                                      nub :: Eq a => [a] -> [a]
                                      nub [] = []
                                      nub (x:xs) = x : nub [ y | y <- xs, y /= x ]

                                      delete :: Eq a => [a] -> [a]
                                      delete y [] = []
                                      delete y (x:xs) = if x == y then xs else x : delete y xs

                                      (\\) :: Eq a => [a] -> [a] -> [a]
                                      (\\) = foldl (flip delete)

                                      union :: Eq a => [a] -> [a] -> [a]
                                      union xs ys = xs ++ (ys \\ xs)

                                      intersect :: Eq a => [a] -> [a] -> [a]
                                      intersect xs ys = [ x | x <- xs, x `elem` ys ]

                                      intersperse :: a -> [a] -> [a]
                                      intersperse p [] = []
                                      intersperse p [x] = [x]
                                      intersperse p (x:xs) = x:p:intersperse p xs
                                      -- intersperse 0 [1,2,3,4] == [1,0,2,0,3,0,4]

                                      partition :: (a -> Bool) -> [a] -> ([a],[a])
                                      partition p xs = (filter p xs, filter (not . p) xs)

                                      group :: Eq a => [a] -> [[a]]
                                      -- group "aapaabbbeee" == ["aa", "p", "aa", "bbb", "eee"]
                                      group :: Eq a => [a] -> [[a]]
                                      group [] = []
                                      group (x:xs) = (zs: group ys) where 
                                                                      zs = take r (x:xs)
                                                                      r = count x (x:xs) 
                                                                      ys = drop r (x:xs)
                                                                      
                                      --conta le occorrenze consecutive di un carattere in una stringa a partire dall'inizio
                                      count :: Eq a => a -> [a] -> Int
                                      count a [] = 0
                                      count a [x] = if (a == x) then 1 else 0
                                      count a (x:xs) = if (a == x) then 
                                                          1 + (count a xs)
                                                          else
                                                          0
                                                          
                                      isPrefixOf, isSuffixOf :: Eq a => [a] -> [a] -> [a] -> Bool
                                      isPrefixOf [] _ = True
                                      isPrefixOf _ [] = False
                                      isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
                                      isSuffixOf x y = reverse x `isPrefixOf` reverse y

                                      sort :: (Ord a) => [a] -> [a]
                                      sort = foldr insert []


                                      insert :: (Ord a) => a -> [a] -> [a] -- Insertion sort
                                      insert x [] = [x]
                                      insert x (y:xs) = if x <= y then x:y:xs else y:insert x xs

-- ### Searching elements

  {- elem is the list membership predicate, usually written in infix form, e.g., x `elem` xs. 
  For the result to be False, the list must be finite; True, however, results from an 
  element equal to x found at a finite index of a finite or infinite list. -}
  elem :: Eq a => a -> [a] -> BoolSource

  {-notElem is the negation of elem.-}
  notElem :: Eq a => a -> [a] -> BoolSource

  {-lookup key assocs looks up a key in an association list. -}
  lookup :: Eq a => a -> [(a, b)] -> Maybe bSource



-- ### Zipping lists

  {-zip takes two lists and returns a list of corresponding pairs. 
  If one input list is short, excess elements of the longer list are discarded.-}
  zip :: [a] -> [b] -> [(a, b)]

  {- zip3 takes three lists and returns a list of triples, analogous to zip. -}
  zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]

  {-zipWith generalises zip by zipping with the function given as the first argument, instead of a tupling function. 
  For example, zipWith (+) is applied to two lists to produce the list of corresponding sums.-}
  zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

  {-The zipWith3 function takes a function which combines three elements, 
  as well as three lists and returns a list of their point-wise combination, analogous to zipWith.-}
  zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]

  {-unzip transforms a list of pairs into a list of first components and a list of second components.-}
  unzip :: [(a, b)] -> ([a], [b])

  {-The unzip3 function takes a list of triples and returns three lists, analogous to unzip. -}
  unzip3 :: [(a, b, c)] -> ([a], [b], [c])



-- ### Strings

  {- lines breaks a string up into a list of strings at newline characters. The resulting strings do not contain newlines-}
  lines :: String -> [String]
    
  {- words breaks a string up into a list of words, which were delimited by white space -}

  words :: String -> [String] -- words "ciao" == ['c','i','a','o']

  {-unlines is an inverse operation to lines. It joins lines, after appending a terminating newline to each.-}
  unlines :: [String] -> String

  {-unwords is an inverse operation to words. It joins words with separating spaces.-}
  unwords :: [String] -> String



-- ### Pairs

    fst :: (a,b) -> a                         -- Returns first element of a pair
    fst(x,y) = x

    snd :: (a,b) -> b                         -- Returns second element of a pair
    snd(x,y) =y

    curry :: ((a, b) -> c)
    curry f x y = f(x,y)                      -- Take a function and two arguments, couples the arguments into a pair

    uncurry ::(a -> b -> c) -> ((a,b) -> c) 
    uncurry f p = f (fst p) (snd p)           -- Take a function and a pair, decouples the pair into two arguments



-- ### Higher Order Functions

    id :: a -> a
    id x = x

    const :: a -> b -> a
    const x _ = x

    (.) :: (b -> c) -> (a -> b) -> a -> c   -- Concatenate functions f.g x = f(g(x))
    f.g = \x -> f(g x)


    flip :: (a-> b -> c) -> b -> a -> c     -- Flip arguments
    flip f x y = f y x

    ($) :: (a -> b) -> a ->                 -- Apply function to an argument: useful in higher-order situations, 
    f $ x =  f x                            -- such as map ($ 0) xs, or zipWith ($) fs xs. 











-------------------------------------------------------------------------- 
-- STANDARD TYPE CLASSES
-------------------------------------------------------------------------- 

class Show a where show :: a -> String

class Eq a where
    (==), (/=) :: a -> a -> Bool
    
class (Eq a) => Ord a where 
    (<), (<=), (>=), (>) :: a -> Bool
    max, min :: a -> a
    
class (Eq a, Show a) => Num a where
    (+), (-), (*) :: a -> a -> a
    negate      :: a -> a
    abs, signum :: a -> a
    fromInteger :: Integer -> a

class (Num a, Ord a) => Real a where 
    toRational :: a -> Rational

class (Real a, Enum a) => Integral a where
    quot, rem :: a -> a -> a
    div, mod :: a -> a -> a
    toInteger :: a -> Integer

class (Num a) => Fractional a where 
    (/) :: a -> a -> a 
    fromRational :: Rational -> a
    
    
    
    
    
-------------------------------------------------------------------------- 
-- CUSTOM TYPE CLASSES
-------------------------------------------------------------------------- 

{-
Listoid  are like lists, and cointain at least one element.
Please define a Haskell type class Listoid that contains all the types having the
following operations:

cons: given an element x and a listoid y, return a listoid having x as its first element 
and y as rest
unit: given an element x returns a listoid containing x
append: given two listoids, returns the listoid concatenation of them
listoidfirst: returns the first element of the listoid
listoidlast: returns the last element of the listoid
listoidrest: returns all the elements but the first of the input listoid - must return 
an error if called on a unit listoid

LL is one of such data structures: type LL represents lists optimized w.r.t.access to the
firest and the last element (e.r. accessing them has constant time complexity). 

Please define LL as an instance of Eq, Listoid and Show.

-}

class Listoid l where
    listoidcons     :: a -> l a -> l a
    listoidunit     :: a -> l a
    listoidappend   :: l a -> l a -> l a
    listoidfirst    :: l a -> a
    listoidlast     :: l a -> a
    listoidrest     :: l a -> l a
    
data LL a = LL ([a], a) deriving (Eq)
    
instance Listoid LL where
    x `listoidcons` LL (xs,y)               = LL (x:xs,y)
    listoidunit x                           = LL ([x], x)
    LL (x1,y1) `listoidappend` LL(x2,y2)    = LL(x1 ++ x2, y2)
    listoidfirst (LL (xs, y))               = head xs
    listoidlast (LL (xs, y))                = y
    listoidrest (LL ([], y))                = error "listoidrest on unit"
    listoidrest (LL (xs, y))                = LL ( tail xs, y )

instance (Show a) => Show (LL a) where
    show (LL (x,y)) =
        (foldl (\x -> \y -> x ++ " " ++ y) "LL" (map show x)) ++ "; Last: " ++ show y        
    
--------------------------------------------------------------------------     
-- MONADIC TYPES
--------------------------------------------------------------------------     


instance Monad Maybe where
    return :: a -> Maybe a
    return a = Just a

    >>= :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing >>= _ = Nothing
    (Just a) >>= f = f a
    
instance Monad [] where
    return :: a -> [a]
    return x = [x]
    
    >>= :: [a] -> (a -> [b]) -> [b]
    xs >>= f = concat (map f xs)
    
    
{- 
Define the unless procedure in the IO Monad. It takes two arguments, a condition c and a body b,
and repeats b unless c becomes true (in a sense, it is a "dual" of while)
-}

unless test action = do {
                ; if (test == False) then 
                    do  {
                ;       action; 
                        unless test action;
                        }
                ; else
                    return ();
                    }    
                    
while test action = do {
                ; if (test == True) then 
                    do  {
                ;       action; 
                        while test action;
                        }
                ; else
                    return ();
                    }



-- Define a procedure getCSV that is used, in the Monad IO, 
-- to get a field in a Comma Separated Values
                
getCSV :: IO String
getCSV = do { 
                c <- getChar -- prendo un carattere
            ;   if (c == ';' || c == ',') then return "" -- verifico che sia un separatore
                else
                    do { -- ne prendo un altro e lo restituisco attaccato
                        ;   l <- getCSV
                        ;   return (c:l)
                        }
            }
            
            
            
            
            
            
            
            
            
{-

"define a simple variant of the state monad, where the state is an integer number, 
and in which every application of a bind increments such number"

-}

-- data definition
data Inc a = Inc (Int -> (Int, a))  -- monad definition
instance Monad Inc where
	return x = Inc (\s -> (s,x))
	Inc a >>= f = Inc (\cnt ->
					let (cnt', v) = a cnt
					Inc v' = f v
					in v' (cnt' + 1))


-- let's try
pass :: Inc Int
pass = return 0

esmm :: Inc Int
esmm = do x <- return 1
		pass
		pass
		x <- return (x+1)
		pass
		pass
		return (x+1)

tryesmm = let Inc a = esmm
		in a 0



-- Se faccio partire tryesmm ottengo (6,3) ovvero 6 passaggi di funzione e 3 perché sono
-- partito da x =1 e sono passato a 3. 
            

    
-------------------------------------------------------------------------- 
-- CUSTOM DATA TYPE
-------------------------------------------------------------------------- 

--trees
    
data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

cell (Leaf a) = a
left (Node a b c) = a
right (Node a b c) = c

isLeaf (Leaf _) = true
isLeaf _ = False

occurs :: (Eq a) => a -> Tree a -> Bool
occurs a (Leaf n) = if (n == a) then True else False
occurs a (Node l v r) = if (a == v) then True 
                        else 
                        occurs a l || occurs a r
                        
leavesNumber :: Tree a -> Int
leavesNumber (Leaf n) = 1
leavesNumber (Node a b c) = leavesNumber a + leavesNumber c

balanced :: Tree a -> Bool
balanced (Node a b c) = if (abs(l-r) > 1) then
                            False
                        else
                            True
                            where l = leavesNumber a 
                                  r = leavesNumber c     
                                  
instance Show a => Show (Tree a) where
    show (Leaf a) = show a
    show (Node a b c) = "<" ++ show a ++ " | " ++ show c ++ ">"
    
mapTreeM f (Leaf a) = do
  b <- f a
  return (Leaf b)

mapTreeM f (Node a b c) = do
  x <- mapTreeM f a
  y <- mapTreeM f b
  z <- mapTreeM f c
  return (Node x y z)
                        
                        
--generic expressions
{- 

Define a datatype Exp to represent generic expressions containing symbols and numbers e.g.

b(b(3,4,5), node(d,e)).

-}

-- Atom here are LEAVES of a tree
-- Exp here is a NODE of a tree
data Atom = No Int | Str String deriving Eq
data Exp = At Atom | Ex Atom [Exp] deriving Eq

{- 

Declare Exp as an instance of Show, such that we can obtain representation exactly like
"b(b(3,4,5),node(d,e))" (i.e. deriving Show is considered unacceptable)

-}

instance Show Atom where
    show (No a) = show a
    show (Str a) = filter (\x -> x /= "") (show a)

instance Show Exp where
    show (At x) = show x
    show (Ex x (y:ys)) = show x ++ "(" ++ show y ++ (concatMap (\t -> "," ++ show t) ys) ++ ")"

{- 

Define a function, called subst, that accepts an expression e and two atoms, x and y, and returns
a new expression e' where every instance of x is replaced by y.

-}

subst :: Exp -> Atom -> Atom -> Exp
subst (At a) b c = if (a == b) then (At c) else (At a)
subst (Ex x y) b c = (Ex (if (x == b) then c else x) (map (\g -> subst g x y) y))                       
                        



-- hashtable
type HashTable key value = [(key, value)]

find :: Eq a => (HashTable a b) -> a -> [b]
find t key = [x | (k, x) <- t, key == k]

                        
                        
--movements                 
data Move = Left | Right | Up | Down    -- This has NO MEANING to the compiler: we must explicit that
                                        -- meaning though functions defined
                                        
move :: Move -> Pos -> Pos
move x Left (x,y) = (x-1, y)
move Right (x,y) = (x+1, y)
move Up (x,y) = (x, y-1)
move Down (x,y) = (x, y+1)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move p)





--list
data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons x xs) = 1 + len xs
    
    
    
    
    
    
-- 1.3
{-
Alfio is experimenting with Haskell, and wants to define a very simple object-oriented-like system,
where an object is just a container of a datum, and a list of "methods", i.e. functions
having signature a->a->a, where a is the type of the datum.

This is an example object defined by Alfio:

myob = Obj (5, [("add", \self -> \x -> self+x),
                ("sqr", \self -> \x -> self*self)]) -- x is ignored
                
Please help Alfio to define both the needed datatype, and the call function for invoking methods.
E.g. call myob "add" -2 should return 3
-}

data Obj a = Obj (a, [(String, (a -> a -> a))])

o :: Obj Int
o = Obj (5,     [("add", \self -> \x -> self+x),
                ("sqr", \self -> \x -> self*self)]) -- x is ignored

findMethod :: Obj a -> String -> Maybe (a -> a -> a) -- non e' detto che riesca a 
                                                     -- trovare il metodo
findMethod (Obj (a, x)) string = findString string x

findString :: String -> [(String,b)] -> Maybe b
findString string [] = Nothing
findString string ((name,func):xs) = if (string == name) then Just func 
                                    else findString string xs

call :: (Obj self) -> String -> self -> self
call (Obj (self, x)) function value = do {method self value}
                                    where Just method = findMethod (Obj (self,x)) function 
                                                    
    




-- Orion, Haskell variant - MPradella, MMXII

data SafeFloat = SF Float Float Float deriving Show

valid :: SafeFloat -> SafeFloat
valid (SF x y z) | x == y = SF x x x
valid (SF x y z) | y == z = SF y y y
valid (SF x y z) | x == z = SF z z z
valid _ = error "unsafe"


instance Eq SafeFloat where 
  x == y = let SF vx _ _ = valid x
               SF vy _ _ = valid y
           in vx == vy
  
applySF1 f (SF x y z) = valid (SF (f x) (f y) (f z))
applySF2 f (SF x y z) (SF x' y' z') = valid (SF (f x x') 
                                             (f y y') 
                                             (f z z'))


instance Num SafeFloat where 
  (+) = applySF2 (+)
  (-) = applySF2 (+)
  (*) = applySF2 (*)
  negate = applySF1 negate
  abs = applySF1 abs
  signum = applySF1 signum
  fromInteger x = let x' = fromInteger x
                  in SF x' x' x'
                     

    
--------------------------------------------------------------------------
-- NUMERICAL FUNCTIONS
--------------------------------------------------------------------------

even, odd :: (Integral a) => a -> Bool
even n  = n `rem` 2 == 0
odd = not . even

--------------------------------------------------------------------------
-- MONADIC FUNCTIONS
--------------------------------------------------------------------------

sequence ::Monad m=>[m a]-> m [a]
sequence  = foldr mcons (return [])
                where mcons p q = do x <- p; xs <- q; return (x:xs)

sequence _ :: Monad m => [m a] -> m()
sequence _ xs = do sequence xs; return ()



-------------------------------------------------------------------------- 
-- FUNCTIONS ON BOOLS
-------------------------------------------------------------------------- 

data Bool = False | True
(&&), (||) :: Bool -> Bool -> Bool

True  && x = x
False && _ = False
True  || _ = True
False || x = x

not :: Bool -> Bool
not True = False
not False = True

--------------------------------------------------------------------------
-- FUNCTIONS ON MAYBE
-------------------------------------------------------------------------- 

data Maybe a = Nothing | Just a
isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

fromJust :: Maybe a -> a
fromJust (Just a) = a

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a)  = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:_) = Just a




--------------------------------------------------------------------------
-- FUNCTIONS ON CHAR
-------------------------------------------------------------------------- 

type String = [Char]

toUpper, toLower :: Char -> Char
--toUpper 'a' == 'A'
--toLower 'Z' == 'z'

digitToInt :: Char -> Int
--digitToInt '8' == 8

intToDigit :: Int -> Char
-- intToDigit 3 == '3'

ord :: Char -> Int
chr :: Int -> Char


---------------------------------------------------------------
-- PARALLELISM
---------------------------------------------------------------

-- SEMIEXPLICIT

par :: a -> b -> b 
par x y = y -- la diff. tra par e seq é che stiamo dicendo al compilatore che sarebbe una
            -- buona idea valutare x in parallelo con y, ma non forziamo il compilatore
            -- a farlo

-- e.g.
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

mkList n = [1..n-1]
relprime x y = gcd x y == 1
euler n = length (filter (relprime n) (mkList n))

sumEuler = sum . (map euler) . mkList
sumFibEuler a b = fib a + sumEuler b -- computazione pesante di sue termini indipendenti

parSumFibEuler a b = f 'par' (e + f) where
                                            f = fib a;
                                            e = sumEuler b;
                                            -- problema: ordine di esecuzione call by
                                            -- need di e + f dipende dal compilatore

--pseq fa eseguire prima l'argomento di sinistra poi quello di destra

parSumFibEulerP a b = f 'par' (e 'pseq' (f+e)) where
                                            f = fib a;
                                            e = sumEuler b;

-- QUICKSORT
sort :: (Ord a) => [a] -> [a]
sort (x:xs) = lesser ++ x:greater
    where lesser  = sort [y | y <- xs, y <  x]
          greater = sort [y | y <- xs, y >= x]
sort _ = []

-- QUICKSORT PARALLELO

import Control.Parallel (par, pseq)

parSort :: (Ord a) => [a] -> [a]
parSort (x:xs)    = force greater `par` (force lesser `pseq`
                                         (lesser ++ x:greater))
    where lesser  = parSort [y | y <- xs, y <  x]
          greater = parSort [y | y <- xs, y >= x]
parSort _         = []


-- FIBONACCI PARALLELO

import Control.Parallel

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

parfib n
  | n < 11    = fib n                           -- For small values of n we use the 
                                                -- sequential version
  | otherwise = f1 `par` (f2 `pseq` (f1+f2))    -- calculate f1 and f2 in parallel, 
                                                -- return the sum as the result
where
  f1 = parfib (n-1)
  f2 = parfib (n-2)




-- Curried Functions

{-
    add :: (Int, Int) -> Int
    add = \(x,y) -> x+y
    
    add :: Int -> (Int -> Int)
    add x = \y -> x + y
    
    add :: Int -> Int -> Int
    add = \x -> \y -> x + y
 -}




