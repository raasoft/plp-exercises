-------------------------------------------------------------------------- 
-- HELLO WORLD

-- STANDARD DATA TYPES
    -- LISTS
    -- ENUMERATIONS
    -- LIST COMPREHENSIONS

-- TYPE CLASS (a.k.a. C++ Function Templates or Java Interfaces)
    
    -- TYPECLASS
        -- DEFAULTS
        -- PREREQUISITES

-- CUSTOM DATA TYPES

    -- DATA
        -- RECORD SYNTAX
        -- TYPE VARIABLES

    -- TYPE

    -- NEWTYPE

-- FUNCTIONS

    -- PATTERN MATCHING
    -- LAMBDAS
    -- LET
    -- WHERE

-- CONTROLLING PROGRAM'S FLOW

    -- IF, THEN, ELSE
    -- DO AND MONADIC CONTEST

-- FUNCTIONS ON LISTS OR ENUMERATIONS

    -- REDUCING LISTS
    -- INDEXING
    -- SPECIAL FOLDS
    -- SUBLISTS
    -- PREDICATES
    -- "SETS" OPERATIONS
    -- SEARCHING ELEMENTS
    -- ZIPPING LISTS
    -- STRINGS
    -- PAIRS
    -- NUMERICAL
    -- HIGHER ORDER FUNCTIONS
    -- CHARACTER AND STRINGS


-- FUNCTIONS ON IMMUTABLE ARRAYS
                                                                                                   
    -- Array construction
    -- Accessing arrays
    -- Incremental array updates


-- STANDARD TYPE CLASSES

-- CUSTOM TYPE CLASSES

    -- LISTOID

-- CUSTOM DATA TYPE

    -- TREE NO. 1
    -- STACK NO. 1
    -- BOOLEAN
    -- MAYBE 
    -- GENERIC EXPRESSIONS
    -- HASHTABLE   
    -- MOVEMENTS         
    -- LIST
    -- OBJECT ORIENTED SYSTEM (Smalltalk)
    -- ORION

-- CREATING MODULES

    -- EXAMPLE NO. 1
    -- EXAMPLE NO. 2
    -- ABSTRACT DATA TYPE (ADT)

-- STANDARD MONADIC TYPES

    -- INTRODUCTION
    -- MAYBE
    -- LIST
    -- STATE
            
-- CUSTOM MONADIC TYPES

    -- POLE (not a variant of the state monad)
    -- STACK NO. 2
    -- COUNTER (a variant of State monad)
    -- TREE NO. 2

-- MONADIC FUNCTIONS

    -- CUSTOM MONADIC FUNCTIONS
    -- UNLESS
    -- getCSV
    -- GREATEST COMMOND DIVISOR
                               
-- PARALLELISM

    -- SEMIEXPLICIT
    -- QUICKSORT
    -- FIBONACCI PARALLELO



-------------------------------------------------------------------------- 


-------------------------------------------------------------------------- 
-- HELLO WORLD
-------------------------------------------------------------------------- 

main = do
  putStrLn "Please enter your name: "
  name <- getLine
  putStrLn ("Hello, " ++ name ++ ", how are you?")








-------------------------------------------------------------------------- 
-- STANDARD DATA TYPES
-------------------------------------------------------------------------- 

-- ### LISTS

    -- Sequence of OMOGENEOUS elements
     
    [1,2,3]         :: [Int]
    ['a', 'b', 'c'] :: [Char]
    "abc"           :: [Char]   -- N.B. "abc" is sugar for ['a', 'b', 'c'] --
    ["abc", "bcd"]  :: [[Char]]
    ["abc", "bcd"]  :: [String] -- N.B. String = [Char]

-- ### ENUMERATIONS
    
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

-- ### LIST COMPREHENSIONS

    -- A list comprehension consists of four types of elements:
    -- generators, guards, local bindings and targets.

    -- A list comprehension creates a list of target values based on the generators and guards given.

    -- e.g.

    allSquares = [x * x | x <- [1..]] -- x <- [1..] generates a list of all Integer 
                                      -- values and puts them in x , one by one.
                                      -- x * x creates each element of the list by multiplying x by itself.

    divisorsOf n = [d | d <- [1..(n ‘div‘ 2)], n ‘mod‘ d == 0]  -- concat of guards

    strange = [(a,z) | a <-[1..3], b <-[1..3],
                       c <- [1..3], let z = min a b,
                       z < c ]  -- a=1, b=1, c=1, z = 1, 1<1? no.
                                -- a=1, b=1, c=2, z = 1, 1<1? no.
                                -- ...
                                -- a=1, b=2, c=1, z = 1, 1<1? no.
                                -- a=1, b=2, c=2, z = 1, 1<2? YES: first couple is (1,1)

    upperCaseLetterOfLists = [c | c <- ['a' .. 'Z'], isUpper c]

    -- Find all occurrences of a particular break value `br` in a list word (indexing from 0):
    idxs word br = [i | (i, c) <- zip [0..] word, c == br] -- first zip all word with indexes, 
                                                         -- then mantain all c==br


    -- Find all pytaghorial terns
    let rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]  

-------------------------------------------------------------------------- 
-- TYPE CLASS (a.k.a. C++ Function Templates or Java Interfaces)
-------------------------------------------------------------------------- 

-- ### TYPE CLASS 

  -- A Haskell function is defined to work on a certain type or set of types and cannot be defined more than once. 
  -- Most languages support the idea of “overloading”, where a function can have different behavior depending on 
  -- the type of its arguments. Haskell accomplishes overloading through class and instance declarations.

  -- IN THIS WAY, TYPE CLASS ARE FUNCTION CONTAINERS: a certain value belons to the class A if it has an operations that is in A class.

  -- `class` defines one or more functions that can be applied to any types which are members (i.e., instances) of that
  -- class. A class is analogous to an interface in Java or C#, and instances to a concrete implementation
  -- of the interface.

  -- A class must be declared with one or more type variables. Technically, Haskell 98 only allows one type variable, 
  -- but most implementations of Haskell support so-called multi-parameter type classes, which allow more than one type variable.
  -- We can define a class which supplies a flavor for a given type:

    class Flavor a where
    flavor :: a -> String

  -- Notice that the declaration only gives the type signature of the function. No implementation is given here 
  --(with some exceptions, see “Defaults” on page 8). Continuing, we can define several instances:

    instance Flavor Bool where
    flavor _ = "sweet"

    instance Flavor Char where
    flavor _ = "sour"

  -- Evaluating flavor True gives:
    > flavor True
    "sweet"

  -- While flavor ’x’ gives:
    > flavor ’x’
    "sour"

-- ### DEFAULTS (Class Defaults) 

  --Default implementations can be given for functions in a class. These are useful when certain functions can be defined in terms of others in
  --the class. A default is defined by giving a body to one of the member functions. The canonical example is  class `Eq`, which defines
  --/= (not equal) in terms of == :
  
    class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    (/=) a b = not (a == b)
  
  -- Recursive definitions can be created. Continuing the `Eq` example, == can be defined in terms of /= :
  
    (==) a b = not (a /= b)
  
  --However, if instances do not provide enough concrete implementations of member functions then any program using those instances will loop

-- ### PREREQUISITES FOR CLASSES

  -- Complex Classes needs to have some preconditions: they are expressed right before the name of the instance.

    -- For example, if we want to create an instance for the Class Eq of a `Tree a`, we need to ensure that type a
    -- must support `Eq a` as well:

      instance (Eq a) => Eq (Tree a) where -- type a must support equality as well

      Leaf a          ==  Leaf b          = a == b
      (Branch l1 r1)  ==  (Branch l2 r2)  = (l1==l2) && (r1==r2)
                   _  ==  _               = False

    -- Another example is Show, so if we want to create an instance for the Class Show of `Tree a` we need to ensure that type a
    -- must support `Show a` as well:

      instance Show a => Show (Tree a) where
      show (Leaf a) = show a
      show (Branch x y) = "<" ++ show x ++ " | " ++ show y ++ ">"




-------------------------------------------------------------------------- 
-- CUSTOM DATA TYPES
-------------------------------------------------------------------------- 

-- ### DATA

  -- So-called algebraic data types can be declared as follows:

      data MyType = MyData1 | MyData2 -- MyValue is a DATA CONSTRUCTORS

    -- MyType is the type’s name, a.k.a. TYPE CONSTRUCTOR. MyData1 and MyData2 are values of the type and are called DATA CONSTRUCTORS. 
    -- Multiple constructors are separated with the ‘|’ character. Note that type and constructor names must start with a capital letter. 

    {- CONSTRUCTORS WITH ARGUMENTS -}

      -- The type above is not very interesting except as an enumeration.
      -- Constructors that take arguments can be declared, allowing more information to be stored:
        data Point = TwoD Int Int | ThreeD Int Int Int

      -- Notice that the arguments for each constructor are TYPE NAMES, not constructors. That means this kind of declaration is illegal:
        data Poly = Triangle TwoD TwoD TwoD     -- WRONG

      -- instead, the Point type must be used:
        data Poly = Triangle Point Point Point  -- CORRECT

    {- TYPE AND CONSTRUCTOR NAMES -}

      -- Type and constructor names can be the same, because they will never be used in a place that would cause confusion. For example:
        data User = User String | Admin String
        -- which declares a type named User with two constructors, User and Admin. Using this type in a function makes the difference clear:
        whatUser (User _)   = "normal user."
        whatUser (Admin _)  = "admin user."
      
        -- Some literature refers to this practice as type punning.


        -- other example:

        data Anniversary = Birthday String Int Int Int       -- name, year, month, day
                           | Wedding String String Int Int Int -- spouse name 1, spouse name 2, year, month, day

        showDate :: Int -> Int -> Int -> String
          showDate y m d = show y ++ "-" ++ show m ++ "-" ++ show d
 
        showAnniversary :: Anniversary -> String
          showAnniversary (Birthday name year month day) =
                          name ++ " born " ++ showDate year month day
 
          showAnniversary (Wedding name1 name2 year month day) =
                          name1 ++ " married " ++ name2 ++ " on " ++ showDate year month day


    {- RECORD SYNTAX -}
        
      {-
        Consider a datatype whose purpose is to hold configuration settings. Usually when you extract members from this type, 
        you really only care about one or possibly two of the many settings. Moreover, if many of the settings have the same 
        type, you might often find yourself wondering "wait, was this the fourth or fifth element?" One thing you could do would 
        be to write accessor functions. Consider the following made-up configuration type for a terminal program:
      
        data Configuration =
          Configuration String          -- user name
                        String          -- local host
                        String          -- remote host
                        Bool            -- is guest?
                        Bool            -- is super user?
                        String          -- current directory
                        String          -- home directory
                        Integer         -- time connected
                    deriving (Eq, Show)
      
        You could then write accessor functions, like (I've only listed a few):
          getUserName (Configuration un _ _ _ _ _ _ _) = un
          getLocalHost (Configuration _ lh _ _ _ _ _ _) = lh
          getRemoteHost (Configuration _ _ rh _ _ _ _ _) = rh
          getIsGuest (Configuration _ _ _ ig _ _ _ _) = ig
          ...
        
        You could also write update functions to update a single element. Of course, now if you add an element to the configuration, 
        or remove one, all of these functions now have to take a different number of arguments. This is highly annoying and is an easy 
        place for bugs to slip in. However, there's a solution. We simply give names to the fields in the datatype declaration, as follows:
      -}
      
        data Configuration =
            Configuration { username      :: String,
                            localhost     :: String,
                            remotehost    :: String,
                            isguest       :: Bool,
                            issuperuser   :: Bool,
                            currentdir    :: String,
                            homedir       :: String,
                            timeconnected :: Integer
                          } -- This will automatically generate the following accessor functions for us:

                            username :: Configuration -> String
                            userName (Configuration un _ _ _ _ _ _ _) = un
                              
                            localhost :: Configuration -> String
                            localHost (Configuration _ lh _ _ _ _ _ _) = lh

                              -- etc...
      {-
        Moreover, it gives us a CONVENIENT UPDATE METHOD. 
        Here is a short example for a "post working directory" and "change directory" like 
        functions that work on Configurations
      -}
      
        changeDir :: Configuration -> String -> Configuration
        changeDir cfg newDir =
            -- make sure the directory exists
            if directoryExists newDir
              
              then -- change our current directory
                   cfg{currentdir = newDir} -- CONVENIENT UPDATE METHOD

              else error "directory does not exist"

        postWorkingDir :: Configuration -> String
          -- retrieve our current directory
        postWorkingDir cfg = currentdir cfg
      
      {-
        So, in general, to update the field x in a datatype y to z, you write:

          y{x=z}. 

        You can change more than one; each should be separated by commas, for instance, y{x=z, a=b, c=d}.
      -}

      {- 
        Argument capture is possible with this syntax, although it gets clunky. Continuing the above, we
        now define a Pixel type and a function to replace values with non-zero green components with all black:
      -}
      data Color = C { red, green, blue :: Int }
      data Pixel = P Color

      -- Color value untouched if green is 0
      setGreen (P col@(C { green = 0 })) = P col
      setGreen _ = P (C 0 0 0)

    {- TYPE VARIABLES -}

      --Declaring so-called polymorphic data types is as easy as adding type variables in the declaration:
        data Slot1 a = Slot1 a | Empty1

        -- This declares a type Slot1 with two constructors, Slot1 and Empty1. The Slot1 constructor can take an argument of
        -- any type, which is represented by the type variable a above. We can also mix type variables and specific types in constructors:
        
        data Slot2 a = Slot2 a Int | Empty2

        --Above, the Slot2 constructor can take a value of any type and an Int value

-- ### TYPE

  {-
    This keyword defines a type synonym (i.e., alias).
    This keyword does not define a new type, like data or newtype. It is useful for documenting code
    but otherwise has no effect on the actual type of a given function or value. For example, a
    Person data type could be defined as:
      
      data Person = Person String String -- the first constructor argument represents their first name and the second their last.

    However, the order and meaning of the two arguments is not very clear. 
    A type declaration can help.
  -}

  type FirstName = String
  type LastName = String

  data Person = Person FirstName LastName

  --Because type introduces a synonym, type check ing is not affected in any way.



-- ### NEW TYPE (a.k.a. hybrid between type and data)
  {-
    sometimes we want to define a type with the same representation of an existing type (like
    type) but having a separate identity in the type system (e.g. we want to define a kind of string
    We have just to use newtype
    e.g.
      newtype Str = Str [Char]
      note: we need to define a data constructor, to distinguish it from String

    While data introduces new values and type just creates synonyms, newtype
    falls somewhere between. The syntax for newtype is quite restricted— only one constructor can be defined, 
    and that constructor can only take one argument. 
  -}

  newtype Home = H String       -- H is not a common String, is `HOME`: the typechecker treats them as entirely new types.
  newtype Work = W String       -- W is not a common String, is `HOME`: the typechecker treats them as entirely new types.
  data Phone = Phone Home Work

  -- The following produces a type error:
  lPhone (Phone hm wk) = Phone (lower hm) (lower wk) -- hm and wk are NOT STRING!
  --Instead, we must use pattern-matching to get to the “values” to which we apply lower:
  lPhone (Phone (H hm) (W wk)) = Phone (H (lower hm)) (W (lower wk)







--------------------------------------------------------------------------
-- FUNCTIONS
-------------------------------------------------------------------------- 

-- ### Pattern Matching 
  
        -- `f` is a pattern which matches anything at all, and binds the f variable to whatever is matched.
        -- (x:xs) is a pattern that matches a NON-EMPTY list which is formed by something 
        -- (which gets bound to the x variable) which was cons'd (by the (:) function) 
        -- onto something else (which gets bound to xs). REMEMBER: THEY MATCH ANYTHING
        map f (x:xs) = f x : map f xs

        -- Here we are goint to match AT LEAST 3 ELEMENTS
        dropThree :: [a] -> [a]
        dropThree (_:_:_:xs) = xs
        dropThree _          = [] -- The catch-all second definition provides a reasonable default[ when lists fail to match the main pattern, 
                                  -- and thus prevents runtime crashes due to pattern match failure.


        -- Matches when the string "y" is given.
        agree1 "y" = "Great!"
        -- Matches when the string "n" is given.
        agree1 "n" = "Too bad."
        -- Matches when string beginning
        -- with 0 given.
        g (0:xs) = True
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


        {-
          [1,2.. n] è una lista, mentre le espressioni con : sono nodi cons
          (come in Scheme), che fungono da struttura della lista
          
          [x,_] è una lista con 2 elementi, x e _

          (_:xs) è un nodo cons, la cui prima componente è _ e la seconda xs,
          dunque è compatibile con una lista [1,2,3], _=1, xs=[2,3]

          (x,_) è una coppia (quindi non c'entra nulla con le liste)
          
          [_:xs] è una lista che contiene un unico elemento, anch'esso una
          lista, che dev'essere compatibile con _:xs. In pratica il match ha
          successo ad es con [[1,2,3]], dove _=1, xs=[2,3]
        -}


-- ### Lambdas

          multBy n = \m -> n * m -- returns a lambda that multiplies two numbers

          isMixedCase str =      -- all :: (a -> Bool) -> [a] -> Bool 
            all (\c -> isSpace c ||
            isLower c ||
            isUpper c) str


-- ### Let

          {- 
            Local functions can be defined within a function using let. The let keyword must always be followed by
            in (except for `do` context, where is implicit). The in must appear in the same column as the let keyword. 
            Functions defined have access to all other functions and variables within the same scope (including those defined by
            let). In this example, mult multiplies its argument n by x, which was passed to the original multiples.
          -}

          multiples x = let mult n = n * x
                        in map mult [1..10]
          
          {-
            let “functions” with no arguments are actually constants and, once evaluated, will not evaluate again for that invocation 
            of the outer function. This is useful for capturing common portions of your function and re-using them. Here is a silly
            example which gives the sum of a list of numbers and their average. The numbers definition captures the list of numbers from
            1 to m, and will only be evaulated once per invocation of listStats; similarly, total and avg are only evaluated once
            per invocation 
          -}

          listStats m = let numbers = [1 .. m]
                            total = sum numbers
                            avg = total / m
                        in "total: " ++ show total ++
                           ", avg: " ++ show avg

-- ### Where

          {-
            Similar to let, where defines local functions and constants. The scope of a where definition is the current function. 
            If a function is broken into multiple definitions through pattern-matching, then the scope of a particular where
            clause only applies to that definition.

            For example, the function result below has a different meaning depending on the arguments given to the function strlen:
          -}
          
          strlen [] = result
          where result = "No string given!"

          strlen f = result ++ " characters long!"
          where result = show (length f)
                                                                    

-- ### Let vs Where
  
          {-
            A where clause can only be defined at the level of a function definition. 
            Usually, that is identical to the scope of let definition. 
            
            The only difference is when guards are being used. The scope of the where
            clause extends over all guards. In contrast, the scope of a let expression is
             only the current function clause and guard, if any.
          -}









--------------------------------------------------------------------------
-- CONTROLLING PROGRAM'S FLOW
-------------------------------------------------------------------------- 

-- ### IF, THEN, ELSE

{-
  Remember, if always “returns” a value. It is an expression, not just a control flow statement. 
  This function tests if the string given starts with a lower case letter and, if so, converts it to upper case:
-}

  -- Use pattern-matching to get first character
  sentenceCase (s:rest) = if isLower s
                          then toUpper s : rest
                          else s : rest
  sentenceCase _ = [] -- Anything else is empty string

  -- Another example
  describeLetter :: Char -> String
  describeLetter c =
      if c >= 'a' && c <= 'z'
          then "Lower case"
          else if c >= 'A' && c <= 'Z'
              then "Upper case"
              else "Not an ASCII letter"

-- ### CASE

  {-
    Case is similar to a switch statement in C# or Java, but can match a pattern: 
    the shape of the value be inginspected.
  -}

  data Choices = First String | Second | Third | Fourth

  whichChoice ch = case ch of
                      First _ -> "1st!"
                      Second -> "2nd!"
                      _ -> "Something else."  



-- ### DO AND MONADIC CONTEST

        {-
          The do keyword indicates that the code to follow will be in a monadic context,
          that is "imperative and sequenced fashion".
          Statements are separated by newlines, assignment is indicated by  <- ,
          and a let form is introduced which does not require the in keyword:
        -}

          do name <- getLine
              let loudName = makeLoud name
              putStrLn ("Hello " ++ loudName ++ "!")
              putStrLn (
                  "Oh boy! Am I excited to meet you, "
                      ++ loudName)

          -- IS THE SAME AS

          do name <- getLine
              let loudName = makeLoud name
              in  do putStrLn ("Hello " ++ loudName ++ "!") -- extra `in` and `do` here!
                     putStrLn (
                         "Oh boy! Am I excited to meet you, "
                             ++ loudName)
          {-

            If we try to run the above code:
              test.hs:4:6:
                The last statement in a 'do' block must be an expression
                  name <- getLine

            WHAT'S GOING ON?

            <- can be used with any action but the last: suffice it to say, whenever you use <- to get the value of an action, 
            Haskell is always expecting another action to follow it. So the very last action better not have any <-s.
          -}

          doGuessing num = do
             putStrLn "Enter your guess:"
             guess <- getLine                       -- assignment: another action will follow it in order to let code compile.
             if (read guess) < num
               then do putStrLn "Too low!"          -- NEW ACTION: DO. Here, we are sequencing two actions: putStrLn and doGuessing. 
                       doGuessing num               -- The first has type IO (), which is fine. 
               else if (read guess) > num           -- The second also has type IO (), which is fine. 
                      then do putStrLn "Too high!"  -- NEW ACTION: DO. The type result of the entire computation is precisely the type of the final computation. 
                              doGuessing num        -- Thus, the type of the "then" branch is also IO (). 
                      else do putStrLn "You Win!"   -- NEW ACTION: DO. A similar argument shows that the type of the "else" branch is also IO ().
                                                    -- This means the type of the entire if/then/else construction is IO (), which is just what we want.
                        




--------------------------------------------------------------------------
-- FUNCTIONS ON LISTS OR ENUMERATIONS
-------------------------------------------------------------------------- 


-- ### Common operations


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
            Append more lists.
          -}  
          concat ::  [[a]] -> [a] -- concat ["foo", "bar", "baz"]    --> "foobarbaz"
            concat = foldl (++) []

          {-
            map f xs is the list obtained by applying f to each element of xs, i.e.,
          -}
          map :: (a -> b) -> [a] -> [b] 
            map f xs = [ f x | x <- xs ] -- cool use of list comprehensions!

            {-
              map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]
              map f [x1, x2, ...] == [f x1, f x2, ...]
            -}

            my_map :: (a->b) -> [a] -> [b] -- Custom implementation of map
              my_map _ [] = []
              smy_map f (x:xs) = f x : my_map f xs

          {-
            revmap f xs is the list obtained by applying f to each element of reverse xs, i.e.,
            It traverses the list more than once!
          -}
          revMap :: (a -> a) -> [a] -> [a]
            revMap f xs = map f (reverse xs)

          my_revMap :: (a -> a) -> [a] -> [a] -- Custom implementation of revMap
            my_revMap f (x:[]) = [f x]
            my_revMap f (x:xs) = ((revMap f xs) ++ [f x])

          {-
            Extract the first element of a list, which must be non-empty.
          -}
          head :: [a] -> a
            head [x] = x
            head (x:_) = x

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
            tail (x:xs) = xs -- tail [1,2,3,4,5] == [2, 3, 4, 5]

          {-
            Return all the elements of a list except the last one. The list must be non-empty.
          -}
          init :: [a] -> [a] --init [1,2,3,4,5] = [1,2,3,4]
            init [x]         =  []  
            init (x:xs)      =  x : init xs  
            init []          =  error "Prelude.init: empty list"

          {-
            Test whether a list is empty.
          -}
          null :: [a] -> Bool
          null [] = True
          null (_:_) = False

          {-
            O(n). length returns the length of a finite list as an Int. 
            It is an instance of the more general genericLength, the result type of which may be any kind of number.
          -}
          length :: [a] -> Int
          length [] = 0
          length (_:xs) = 1 + length xs

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
          foldl :: (a -> b -> a) -> a -> [b] -> a
          foldl f z [] = z
          foldl f z (x:xs) = foldl f (f z x) xs -- foldl f z [x1, x2, ..., xn] = (...((z `f` x1) `f` x2) `f`...) `f` xn

          {- 
          foldl1 is a variant of foldl that has no starting value argument, and thus must be applied to non-empty lists. 

          e.g.
          my_sum2 :: (Num a) => [a] -> a
          my_sum2 xs = foldl1 (+) xs  

          my_sum2 is EQUIVALENT to sum
          -}
          foldl1 :: (a -> a -> a) -> [a] -> a
          foldl1 f (x:xs)  =  foldl f x xs
          foldl1 _ []      =  error "Prelude.foldl1: empty list"

          my_fold_left :: (a -> b -> a) -> a -> [b] -> a -- Custom implementation of foldl: fold-left(op, e, [e1, e2, ..., en]) = (((e op e1) op e2) op ... en)
          my_fold_left op x [y] = op x y
          my_fold_left op x (y:ys) = my_fold_left op (op x y) ys
                                              
          {- 
          foldr, applied to a binary operator, a starting value (typically the right-identity of the operator), 
          and a list, reduces the list using the binary operator, from right to left: 
          -}
          foldr :: (a -> b -> b) -> b -> [a] -> b
          foldr f z [] = z
          foldr f z (x:xs) = f x (foldr f z xs) -- foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)

          my_fold_right :: (a -> b -> b) -> b -> [a] -> b -- Custom implementation of foldr: fold-right(op, e, [e1, e2, ..., en]) = (e1 op ( e2 op ... (en op e)))
          my_fold_right op x [y] = op y x 
          my_fold_right op x (y:ys) = op y (my_fold_right op x ys)                                       

          {-
          foldr1 is a variant of foldr that has no starting value argument, and thus must be applied to non-empty lists.
          -}
          foldr1 :: (a -> a -> a) -> [a] -> a
          foldr1 f [x]     =  x
          foldr1 f (x:xs)  =  f x (foldr1 f xs)
          foldr1 _ []      =  error "Prelude.foldr1: empty list"


-- ### Indexing

          import Data.List

          {-
            List index (subscript) operator, starting from 0. 
            It is an instance of the more general genericIndex, which takes an index of any integral type.
          -}
          (!!) :: [a] -> Int -> a
            (x:_)  !! 0 = x
            (_:xs) !! n = xs !! (n-1)

          {-
            The find function takes a predicate and a list and returns the first element in the list matching the predicate, or Nothing if there is no such element.
          -}
          find :: (a -> Bool) -> [a] -> Maybe a
          find p          = listToMaybe . filter p

          {- The 'findIndex' function takes a predicate and a list and returns the index of the first element in the list satisfying the predicate,
            or 'Nothing' if there is no such element.
          -}
          findIndex       :: (a -> Bool) -> [a] -> Maybe Int
          findIndex p     = listToMaybe . findIndices p

          {- 
            The 'findIndices' function extends 'findIndex', by returning the indices of all elements satisfying the predicate, in ascending order. 
          -}
          findIndices      :: (a -> Bool) -> [a] -> [Int]
          findIndices p xs = [ i | (x,i) <- zip xs [0..], p x]

          {- 
            In the given list which is equal (by '==') to the query element, or 'Nothing' if there is no such element.
          -}
          elemIndex  :: Eq a => a -> [a] -> Maybe Int
          elemIndex x = findIndex (x==)

          {- 
            The 'elemIndices' function extends 'elemIndex', by returning the indices of all elements equal to the query element, in ascending order.
          -}
          elemIndices :: Eq a => a -> [a] -> [Int]
          elemIndices x = findIndices (x==)


-- ### Special folds


          {-
            and returns the conjunction of a Boolean list. For the result to be True, the list must be finite; 
            False, however, results from a False value at a finite index of a finite or infinite list.
          -}
          and :: [Bool] -> Bool
          and = foldr (&&) True                                   
                               
          {- 
            or returns the disjunction of a Boolean list. For the result to be False, the list must be finite; 
            True, however, results from a True value at a finite index of a finite or infinite list.
          -}
          or :: [Bool] -> Bool
          or = foldr (||) False

          {-
            Applied to a predicate and a list, any determines if any element of the list satisfies the predicate. 
            For the result to be False, the list must be finite; 
            True, however, results from a True value for the predicate applied to an element at a finite index of a finite or infinite list.
          -}
          any :: (a -> Bool) -> [a] -> Bool
            any p = or . map p

          {-
            Applied to a predicate and a list, all determines if all elements of the list satisfy the predicate. 
            For the result to be True, the list must be finite; 
            False, however, results from a False value for the predicate applied to an element at a finite index of a finite or infinite list.
          -}
          all :: (a -> Bool) -> [a] -> Bool
            all p = and . map p

          {- 
            The sum function computes the sum of a finite list of numbers. 
          -}
          sum :: Num a => [a] -> a
            sum = fold (+) 0                  

          my_sum :: (Num a) => [a] -> a  
          my_sum [] = 0  
          my_sum (x:xs) = x + my_sum xs                   

          my_sum2 :: (Num a) => [a] -> a
          my_sum2 xs = foldl1 (+) xs  
          
          {- 
            The product function computes the product of a finite list of numbers.
          -}
          product :: Num a => [a] -> a
            product = foldl (*) 1
          {-
            Concatenate a list of lists.
          -}
          concat :: [[a]] -> [a]
            concat xss = foldr (++) [] xss

          {-
            Map a function over a list and concatenate the results.
          -}
          concatMap :: (a -> [b]) -> [a] -> [b]
            concatMap f = concat . map f

          {-
            maximum returns the maximum value from a list, which must be non-empty, finite, and of an ordered type. 
            It is a special case of maximumBy, which allows the programmer to supply their own comparison function.
          -}
          maximum :: Ord a => [a] -> a
            maximum [] = error "Prelude.maximum: empty list"
            maximum xs = foldl1 max xs

          {-
            minimum returns the minimum value from a list, which must be non-empty, finite, and of an ordered type. 
            It is a special case of minimumBy, which allows the programmer to supply their own comparison function. 
          -}
          minimum :: Ord a => [a] -> a
            minimum [] = error "Prelude.minimum: empty list
            minimum xs = foldl1 min xs          
                                                
          {-
            The mapAccumL function behaves like a combination of map and foldl; it applies a function to each 
            element of a list, passing an accumulating parameter from left to right, and returning a final value 
            of this accumulator together with the new list.
          -}
          mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
          

          {-
            The mapAccumR function behaves like a combination of map and foldr; it applies a function to each 
            element of a list, passing an accumulating parameter from right to left, and returning a final value 
            of this accumulator together with the new list.
          -}
          mapAccumR :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
  


-- ### Sublists

          {-
            take n, applied to a list xs, returns the prefix of xs of length n, or xs itself if n > length xs.
          -}
          take :: Int -> [a] -> [a]                                 
            take n list@(x:xs) =
               | n == length list = list
               | otherwise = x ++ take (n-1) xs

            take n _ | n <= 0 = [] -- Other implementation
              take _ [] = []
              take n (x:xs) = x : take (n-1) xs

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
            drop n list@(_:xs) = 
              | n <= 0 = xs
              | otherwise = drop (n-1)     

            drop _ [] = [] -- Other implementation
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
            splitAt n xs = (take n xs, drop n xs)
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
            takeWhile p [] = []
            takeWhile p (x:xs)  | p x = x : takeWhile p xs
                                | otherwise = []
            -- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
            -- takeWhile (< 0) [1,2,3] == []    

          {-
            dropWhile p xs returns the suffix remaining after takeWhile p xs
          -}
          dropWhile :: (a -> Bool) -> [a] -> [a]
            dropWhile p [] = []
            dropWhile p xs@(x:xss)  | p x = dropWhile p xss
                                    | otherwise = xs
            -- dropWhile (< 3) [1,2,3,4,5,1,2,3] = [3,4,5,1,2,3]
            -- dropWhile (< 0) [1,2,3] = [1,2,3]


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
            iterate f a = a : iterate f (f a)-- iterate f x = [x, f x, f (f x), ...]

          {- 
            reDo f x returns an infinite repeated applications of f to x:
          -}
          iterate :: (a -> a) -> a -> a
            iterate f x = x : iterate f (f x)                                  

          {-
            repeat x is an infinite list, with x the value of every element. 
          -}
          repeat :: a -> [a]
          repeat x = xs where xs = x:xs

          {- 
            replicate n x is a list of length n with x the value of every element. It is an instance of the more general genericReplicate, in which n may be of any integral type. 
          -}
          replicate :: Int -> a -> [a]
            replicate n x = take n (repeat x)

          {-
            cycle ties a finite list into a circular one, or equivalently, the infinite repetition of the original list. It is the identity on infinite lists. 
          -}
          cycle :: [a] -> [a]
            cycle :: [a] -> [a]
            cycle [] = error "Prelude.cycle: empty list"
            cycle xs = xss where xss = xs ++ xss          -- Note: lazy evaluation!

         takeWhile :: (a -> Bool) -> [a] -> [a] -- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
          takeWhile p [] = []
          takeWhile p (x:xs)  | p x = x : takeWhile p xs
                              | otherwise = []
                                              


          {-
            dropWhile p xs returns the suffix remaining after takeWhile p xs
          -}
          dropWhile :: (a -> Bool) -> [a] -> [a] --  dropWhile (< 3) [1,2,3,4,5,1,2,3] == [3,4,5,1,2,3]
          dropWhile p [] = []
          dropWhile p xs@(x:xss)  | p x = dropWhile p xss
                                  | otherwise = xs
                                              
          {-
            The dropWhileEnd function drops the largest suffix of a list in which the given predicate holds for all elements. 
          -}
          dropWhileEnd :: Eq a => [a] -> [a] -> Maybe [a]                                      

           dropWhileEnd isSpace "foo\n" == "foo"
           dropWhileEnd isSpace "foo bar" == "foo bar"
           dropWhileEnd isSpace ("foo\n" ++ undefined) == "foo" ++ undefined

          {-
            The stripPrefix function drops the given prefix from a list. It returns Nothing if the list did not start with the 
            prefix given, or Just the list after the prefix, if it does.
          -}
          stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]

           stripPrefix "foo" "foobar" == Just "bar"
           stripPrefix "foo" "foo" == Just ""
           stripPrefix "foo" "barfoo" == Nothing
           stripPrefix "foo" "barfoobaz" == Nothing


          {-
            The inits function returns all initial segments of the argument, shortest first. For example,
            Note that inits has the following strictness property: inits _|_ = [] : _|_
          -}
          inits :: [a] -> [[a]]
          inits g = take (length g) (iterate myinit g)
            myinit :: [a] -> [a] --init [1,2,3,4,5] = [1,2,3,4]
            myinit [x]         =  []  
            myinit (x:xs)      =  x : init xs  
            myinit []          =  []
            -- inits "abc" == ["","a","ab","abc"]


          {-
            The tails function returns all final segments of the argument, longest first. For example,
          -}
          tails :: [a] -> [[a]] -- Note that tails has the following strictness property: tails _|_ = _|_ : _|_
          tails g = take (length g) (iterate mytail g)
            mytail :: [a] -> [a]
            mytail [x]         =  [x]  
            mytail (x:xs)      =  xs
            mytail []          =  []
            -- tails "abc" == ["abc", "bc", "c",""]


          {-
            The group function takes a list and returns a list of lists such that the concatenation of the result is equal 
            to the argument. Moreover, each sublist in the result contains only equal elements.

            group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
          -}
          group :: Eq a => [a] -> [[a]]
          group [] = []
          group (x:xs) = (zs: group ys) where 
                                          zs = take r (x:xs)
                                          r = count x (x:xs) 
                                          ys = drop r (x:xs)

          {-
            Counts consecutive occurrencies of a character in a string starting from the begin
          -}
          count :: Eq a => a -> [a] -> Int
            count a [] = 0
            count a [x] = if (a == x) then 1 else 0
            count a (x:xs) = if (a == x) then 
                                1 + (count a xs)
                                else
                                0
                                  


-- ### Predicates

          {- 
            The isPrefixOf function takes two lists and returns True iff the first list is a prefix of the second.
          -}
          isPrefixOf :: Eq a => [a] -> [a] -> Bool
          isPrefixOf [] _ = True
          isPrefixOf _ [] = False
          isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
          

          myIsPrefix subStr str = if find (==subStr) (inits str) == Nothing then False else True

          {-
            The isSuffixOf function takes two lists and returns True iff the first list is a suffix of the second. Both lists must be finite.
          -}
          isSuffixOf :: Eq a => [a] -> [a] -> Bool
          isSuffixOf x y = reverse x `isPrefixOf` reverse y

          myIsSuffix subStr str = if find (==subStr) (tails str) == Nothing then False else True
          
          {-
            The isInfixOf function takes two lists and returns True iff the first list is contained, wholly and intact, anywhere within the second.
          -}
          isInfixOf :: Eq a => [a] -> [a] -> Bool
          isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
          -- isInfixOf "Haskell" "I really like Haskell." == True
          -- isInfixOf "Ial" "I really like Haskell." == False

          
          myIsInfix subStr str = if length (filter (isPrefix subStr) (tails str)) == 0 then False else True

-- ### SET operations

          {-
            The \\ function is list difference (non-associative). 
            In the result of xs \\ ys, the first occurrence of each element of ys in turn (if any) has been removed from xs.
          -}
          (\\) :: Eq a => [a] -> [a] -> [a]
          (\\) = foldl (flip delete)

          {-
            delete x removes the first occurrence of x from its list argument. For example:
            delete 'a' "banana" == "bnana"
          -}
          delete :: Eq a => [a] -> [a]
          delete y [] = []
          delete y (x:xs) = if x == y then xs else x : delete y xs

          {-
            O(n^2). The nub function removes duplicate elements from a list. In particular, it keeps only the first occurrence of each element. 
            (The name nub means `essence'.) It is a special case of nubBy, which allows the programmer to supply their own equality test.
          -}
          nub :: Eq a => [a] -> [a]
          nub [] = []                                
          nub (x:xs) = x : nub [ y | y <- xs, y /= x ] -- cool way of using list comprehension!
          {-
            The union function returns the list union of the two lists. For example, "dog" `union` "cow" == "dogcw"
            Duplicates, and elements of the first list, are removed from the the second list, but if the first list contains duplicates, 
            so will the result. It is a special case of unionBy, which allows the programmer to supply their own equality test.
          -}
          union :: Eq a => [a] -> [a] -> [a]
          union xs ys = xs ++ (ys \\ xs)        
                                              
          {-
            The intersect function takes the list intersection of two lists. For example, [1,2,3,4] `intersect` [2,4,6,8] == [2,4]
            [1,2,2,3,4] `intersect` [6,4,4,2] == [2,2,4].
            If the first list contains duplicates, so will the result.
            It is a special case of intersectBy, which allows the programmer to supply their own equality test. 
            If the element is found in both the first and the second list, the element from the first list will be used.
          -}
          intersect :: Eq a => [a] -> [a] -> [a]
            intersect xs ys = [ x | x <- xs, x `elem` ys ]

          {-
            Inframmezza un valore in una lista
            -- intersperse 0 [1,2,3,4] == [1,0,2,0,3,0,4]  
          -}
          intersperse :: a -> [a] -> [a]
            intersperse p [] = []
            intersperse p [x] = [x]
            intersperse p (x:xs) = x:p:intersperse p xs

          {-
            Inframmezza una lista in una lista di liste 
            -- intersperse [0] [[1,2,3,4],[1,2,3,4]] == [[1,2,3,4],[0],[1,2,3,4],[0]]
          -}
          intercalate :: [a] -> [[a]] -> [a]
          intercalate xs xss = (concat (intersperse xs xss))


          {-
            The transpose function transposes the rows and columns of its argument. For example,
            transpose [[1,2,3],[4,5,6]] == [[1,4],[2,5],[3,6]]
          -}
          transpose :: [[a]] -> [[a]]
          transpose ([]:_) = []  
          transpose x = (map head x) : transpose (map tail x)

          {-
            The subsequences function returns the list of all subsequences of the argument.
            subsequences "abc" == ["","a","b","ab","c","ac","bc","abc"]
          -} 
          subsequences :: [a] -> [[a]]
          subsequences xs         =  [] : nonEmptySubsequences xs

                nonEmptySubsequences         :: [a] -> [[a]]
                nonEmptySubsequences []      =  []
                nonEmptySubsequences (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences xs)
                                              where f ys r = ys : (x : ys) : r


          {-
            The permutations function returns the list of all permutations of the argument.
            permutations "abc" == ["abc","bac","cba","bca","cab","acb"]
          -}
          permutations :: [a] -> [[a]]
          permutations xs0        =  xs0 : perms xs0 []
            where
              perms []     _  = []
              perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
                where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
                      interleave' _ []     r = (ts, r)
                      interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                               in  (y:us, f (t:y:us) : zs)

          {-
            This function performs purely a cartesian product of the list passed to it: [[1,2,3],[4,5,6],[7,8,9]]

            -- result: 
            -- [[1,4,7],[1,4,8],[1,4,9],[1,5,7],[1,5,8],[1,5,9],[1,6,7],[1,6,8],[1,6,9],
            --  [2,4,7],[2,4,8],[2,4,9],[2,5,7],[2,5,8],[2,5,9],[2,6,7],[2,6,8],[2,6,9],
            --  [3,4,7],[3,4,8],[3,4,9],[3,5,7],[3,5,8],[3,5,9],[3,6,7],[3,6,8],[3,6,9]]
            -- to obtain the same result without purely functions, please use `sequence [[1,2,3],[4,5,6],[7,8,9]]`

          -}
          cartesianProduct :: [[a]] -> [[a]]
          cartesianProduct list = foldr k [[]] list where k m mx = [(x:xs) | x <- m, xs <- mx]
                                                        
          {-
            Order a list using insertion sort 
          -}
          insertionSort :: (Ord a) => a -> [a] -> [a] -- Insertion sort
          insertionSort x [] = [x]
          insertionSort x (y:xs) = if x <= y then x:y:xs else y:insert x xs

          sort :: (Ord a) => [a] -> [a]
          sort = foldr insertionSort []



                                      

                                      

  -- ### Curried Functions

          add :: (Int, Int) -> Int
          add = \(x,y) -> x+y
          
          add :: Int -> (Int -> Int)
          add x = \y -> x + y
          
          add :: Int -> Int -> Int
          add = \x -> \y -> x + y
                                                                             

                                                          


                                      


  

-- ### Searching elements

          {- elem is the list membership predicate, usually written in infix form, e.g., x `elem` xs. 
          For the result to be False, the list must be finite; True, however, results from an 
          element equal to x found at a finite index of a finite or infinite list. -}
          elem :: Eq a => a -> [a] -> Bool
          elem x = any (== x)

          {-notElem is the negation of elem.-}
          notElem :: Eq a => a -> [a] -> Bool
          notElem x = all (/= x)

          {-lookup key assocs looks up a key in an association list. -}
          lookup :: Eq a => a -> [(a, b)] -> Maybe b
            lookup key [] = Nothing
            lookup key ((x,y):xys)  | key == x = Just y
                                    | otherwise = lookup key xys

          {-
            filter, applied to a predicate and a list, returns the list of those elements that satisfy the predicate;
          -}
          filter :: (a -> Bool) -> [a] -> [a]
            filter p xs = [ x | x <- xs, p x] -- filter(p, [e1, e2, ..., en]) = [ei | 1<=n<=n, p(ei)]

         
          my_filter :: (a -> Bool) -> [a] -> [a]  -- Custom reimplementation of filter
          my_filter _ [] = []
          my_filter p (x:xs)  | p x = x : my_filter p xs
                    | not (p x) = my_filter p xs

          {-
            The partition function takes a predicate a list and returns the pair of lists of elements which do and do not satisfy the predicate, respectively; i.e.,
          -}
          partition :: (a -> Bool) -> [a] -> ([a], [a])
          partition p xs = (filter p xs, filter (not . p) xs)

-- ### Zipping lists

          {-zip takes two lists and returns a list of corresponding pairs. 
          If one input list is short, excess elements of the longer list are discarded.-}
          zip :: [a] -> [b] -> [(a, b)]
            zip = zipWith (,)

          {- zip3 takes three lists and returns a list of triples, analogous to zip. -}
          zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]

          {-zipWith generalises zip by zipping with the function given as the first argument, instead of a tupling function. 
          For example, zipWith (+) is applied to two lists to produce the list of corresponding sums.-}
          zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
            zipWith z (a:as) (b:bs) = z a b : zipWith z as bs
            zipWith _ _ _ = []

          {-The zipWith3 function takes a function which combines three elements, 
          as well as three lists and returns a list of their point-wise combination, analogous to zipWith.-}
          zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]

          {-unzip transforms a list of pairs into a list of first components and a list of second components.-}
          unzip :: [(a, b)] -> ([a], [b])
            unzip = foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])

          {-The unzip3 function takes a list of triples and returns three lists, analogous to unzip. -}
          unzip3 :: [(a, b, c)] -> ([a], [b], [c])                                     

        -- ### Strings

          {- lines breaks a string up into a list of strings at newline characters. The resulting strings do not contain newlines-}
          lines :: String -> [String]
          lines ""                =  []
          lines s                 =  let (l, s') = break (== '\n') s
                                   in  l : case s' of
                                                []      -> []
                                                (_:s'') -> lines s''

          {- words breaks a string up into a list of words, which were delimited by white space -}

          words :: String -> [String] -- words "ciao" == ['c','i','a','o']
          words s                 =  case dropWhile {-partain:Char.-}isSpace s of
                                          "" -> []
                                          s' -> w : words s''
                                                where (w, s'') =
                                                       break {-partain:Char.-}isSpace s'

          {-unlines is an inverse operation to lines. It joins lines, after appending a terminating newline to each.-}
          unlines :: [String] -> String

          {-unwords is an inverse operation to words. It joins words with separating spaces.-}
          unwords :: [String] -> String
          unwords []              =  ""
          unwords [w]             = w
          unwords (w:ws)          = w ++ ' ' : unwords ws


-- ### Pairs

          fst :: (a,b) -> a                         -- Returns first element of a pair
          fst(x,y) = x

          snd :: (a,b) -> b                         -- Returns second element of a pair
          snd(x,y) =y

          curry :: ((a, b) -> c)
          curry f x y = f(x,y)                      -- Take a function and two arguments, couples the arguments into a pair

          uncurry ::(a -> b -> c) -> ((a,b) -> c) 
          uncurry f p = f (fst p) (snd p)           -- Take a function and a pair, decouples the pair into two arguments


-- ### Numerical

          even :: (Integral a) => a -> Bool
          even n  = n `rem` 2 == 0

          odd :: (Integral a) => a -> Bool
          odd = not . even

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




-- ### Characters and String

  type String = [Char]

  -- To convert a number to a string use: 
  show :: Show a => a -> String
    -- show NUMBER 
      -- show 3 --> "3"
      -- show 33 --> "33"
      -- show "333" ---> "\"333\""

  read :: Read a => String -> a
  -- To convert a string to a type use: 
    -- read "STRING" :: TypeCast 
      -- read "3" :: Int --> 3
      -- read "3" :: Float --> 3.0
      -- read "3.0" :: Int --> ERRORE impossibile convertire in questo modo da float a intero

  charToString :: Char -> String -- If you want to concat a string with a char: "ciao" ++ charToString('l')
  charToString c = [c]

  toUpper, toLower :: Char -> Char  -- e.g. toUpper 'a' == 'A'
                                    -- e.g. toLower 'Z' == 'z'

  digitToInt :: Char -> Int         -- e.g. digitToInt '8' == 8
  digitToInt c
   | isDigit c            =  ord c - ord '0'
   | c >= 'a' && c <= 'f' =  ord c - ord 'a' + 10
   | c >= 'A' && c <= 'F' =  ord c - ord 'A' + 10
   | otherwise            =  error ("Char.digitToInt: not a digit " ++ show c) -- sigh


  -- IT WORKS ONLY FOR A SINGLE INT (0-15) from int to hexadecimal
  intToDigit :: Int -> Char                                         -- e.g. intToDigit 3 == '3'
  intToDigit i =                                                    -- e.g. intToDigit 15 = 'F'
  | i >= 0  && i <=  9   =  toEnum (fromEnum '0' + i)
  | i >= 10 && i <= 15   =  toEnum (fromEnum 'a' + i - 10)
  | otherwise            =  error "Char.intToDigit: not a digit"

  ord :: Char -> Int -- ord '3' == 51 (ascii code)
  ord  =  fromEnum
     
  chr :: Int -> Char -- chr 51 == '3' (ascii code)
  chr  =  toEnum














--------------------------------------------------------------------------
-- FUNCTIONS ON IMMUTABLE ARRAYS
-------------------------------------------------------------------------- 
                                                                                                   
{- Immutable non-strict arrays
Haskell provides indexable arrays, which may be thought of as functions whose domains are isomorphic to contiguous subsets of the integers. Functions restricted in this way can be implemented efficiently; in particular, a programmer may reasonably expect rapid access to the components. To ensure the possibility of such an implementation, arrays are treated as data, not as general functions.
-}

import Data.Array

  -- ### Array construction
    -- Construct an array with the specified bounds and containing values for given indices within these bounds.
    array :: Ix i => (i, i)     -- a pair of bounds, each of the index type of the array. These bounds are the lowest and highest indices in the array, in that order. 
                                -- For example, a one-origin vector of length '10' has bounds '(1,10)', and a one-origin '10' by '10' matrix has bounds '((1,1),(10,10))'.
    -> [(i, e)]                 -- a list of associations of the form (index, value). Typically, this list will be expressed as a comprehension. 
                                -- An association '(i, x)' defines the value of the array at index i to be x.
    -> Array i e  
        {-
          Because the indices must be checked for these errors, array is strict in the bounds argument and in the indices of the association list, 
          but nonstrict in the values. Thus, recurrences such as the following are possible:

              a = array (1,100) ((1,1) : [(i, i * a!(i-1)) | i <- [2..100]])

          Not every index within the bounds of the array need appear in the association list, but the values associated with indices that do not appear will be undefined (i.e. bottom).
          If, in any dimension, the lower bound is greater than the upper bound, then the array is legal, but empty. 
          Indexing an empty array always gives an array-bounds error, but bounds still yields the bounds with which the array was constructed.
        -}

  -- Construct an array from a pair of bounds and a list of values in index order.
  {-
    listArray (0,10) [  (Root, "R"),
                      (Parent 0, "A"),
                      (Parent 0, "B"),
                      (Parent 1, "C"),
                      (Parent 1, "D"),
                      (Parent 1, "E"),
                      (Parent 2, "F"),
                      (Root    , "W"),
                      (Parent 7, "X"),
                      (Parent 7, "Y"),
                      (Parent 7, "Z")
                   ]
  -}
  listArray :: Ix i => (i, i) -> [e] -> Array i e


  -- The accumArray deals with repeated indices in the association list using an accumulating function which combines the values of associations with the same index. 
  -- For example, given a list of values of some index type, hist produces a histogram of the number of occurrences of each index within a specified range:
  {-
    hist :: (Ix a, Num b) => (a,a) -> [a] -> Array a b
    hist bnds is = accumArray (+) 0 bnds [(i, 1) | i<-is, inRange bnds i]
    If the accumulating function is strict, then accumArray is strict in the values, as well as the indices, in the association list. 
    Thus, unlike ordinary arrays built with array, accumulated arrays should not in general be recursive.
  -}
  accumArray :: Ix i
  => (e -> a -> e)      -- accumulating function
  -> e                  -- initial value
  -> (i, i)             -- bounds of the array
  -> [(i, a)]           -- association list
  -> Array i e  


-- ### Accessing arrays

  -- The value at the given index in an array.
  (!) :: Ix i => Array i e -> i -> e

  -- The bounds with which an array was constructed.
  bounds :: Ix i => Array i e -> (i, i)

  -- The list of indices of an array in ascending order.
  indices :: Ix i => Array i e -> [i]

  -- The list of elements of an array in index order.
  elems :: Ix i => Array i e -> [e]

  -- The list of associations of an array in index order.
  assocs :: Ix i => Array i e -> [(i, e)]

-- ### Incremental array updates

  -- Constructs an array identical to the first argument except that it has been updated by the associations in the right argument. 
  {-
    Repeated indices in the association list are handled as for array: Haskell 98 specifies that the resulting array is undefined (i.e. bottom), 
    but GHC's implementation uses the last association for each index.
  -}

  (//) :: Ix i => Array i e -> [(i, e)] -> Array i e

    -- For example, if m is a 1-origin, n by n matrix, then:
     m//[((i,i), 0) | i <- [1..n]]
    -- is the same matrix, except with the diagonal zeroed.

  -- accum f takes an array and an association list and accumulates pairs from the list into the array with the accumulating function f. 
  -- Thus accumArray can be defined using accum: accumArray f z b = accum f (array b [(i, z) | i <- range b])
  accum :: Ix i => (e -> a -> e) -> Array i e -> [(i, a)] -> Array i e                                                                                           
                                                                                                                                                            




















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
-- CUSTOM DATA TYPE
-------------------------------------------------------------------------- 

--- ### Trees No. 1 (Simple version)

  -- Definition
    
      data Tree a = Leaf a | Node (Tree a) a (Tree a)

      t :: Tree Int
      t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

    -- Useful Functions

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
                                      
  -- Instances of trees for Show

    instance Show a => Show (Tree a) where
        show (Leaf a) = show a
        show (Node a b c) = "<" ++ show a ++ " | " ++ show c ++ ">"
    
  -- Advanced functions
       
    mapTreeM f (Leaf a) = do
      b <- f a
      return (Leaf b)

    mapTreeM f (Node a b c) = do
      x <- mapTreeM f a
      y <- mapTreeM f b
      z <- mapTreeM f c
    return (Node x y z)
                          

--- ### Stack implementation No. 1 (without Monads)

    {-
      We'll use a list to represent our stack and the head of the list will be the top of the stack. 
      To help us with our task, we'll make two functions: pop and push. 
      pop will take a stack, pop one item and return that item as the result and also return a new stack, without that item. 
      push will take an item and a stack and then push that item onto the stack. It will return () as its result, along with a new stack.
    -}

    type Stack = [Int]  
      
    pop :: Stack -> (Int,Stack)  
    pop (x:xs) = (x,xs)  
      
    push :: Int -> Stack -> ((),Stack)  
    push a xs = ((),a:xs)  
                        
    -- Let's write a small piece of code to simulate a stack using these functions. 
    -- We'll take a stack, push 3 to it and then pop two items, just for kicks. Here it is:

    stackManip :: Stack -> (Int, Stack)  
    stackManip stack = let  
        ((),newStack1) = push 3 stack  
        (a ,newStack2) = pop newStack1  
        in pop newStack2  

        -- stackManip [5,8,2,1]  ----->  (5,[8,2,1])  

        {-
            The above code for stackManip is kind of tedious since we're manually giving the state to every stateful computation 
            and storing it and then giving it to the next one. Wouldn't it be cooler if, instead of giving the stack manually to 
            each function, we could write something like this: 

            stackManip = do  
                          push 3  
                          a <- pop  
                          pop  

            Simpler? OK! Now go to the Monadic version of stacks!
        -}


-- ### Boolean data type

  data Bool = False | True
  (&&), (||) :: Bool -> Bool -> Bool

  True  && x = x
  False && _ = False
  True  || _ = True
  False || x = x

  not :: Bool -> Bool
  not True = False
  not False = True



-- ### Maybe data type

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



-- ### Generic Expressions

  {- 
    Define a datatype Exp to represent generic expressions containing symbols and numbers e.g.
    b(b(3,4,5), node(d,e)).
  -}

    data Atom = No Int | Str String deriving Eq     -- Atom here are LEAVES of a tree
    data Exp = At Atom | Ex Atom [Exp] deriving Eq  -- Exp here is a NODE of a tree

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
                        



-- ### Hashtable

  type HashTable key value = [(key, value)]

  find :: Eq a => (HashTable a b) -> a -> [b]
  find t key = [x | (k, x) <- t, key == k]
    
                        
-- ### Movements                 

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


-- ### List
  
  data List a = Nil | Cons a (List a)

  len :: List a -> Int
  len Nil = 0
  len (Cons x xs) = 1 + len xs

    
    
-- ### Object-Oriented-System (Smalltalk)    
    
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
                                                    
    


-- ### Orion, Haskell variant - MPradella, MMXII

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











---------------------------------------------------------------
-- CREATING MODULES
---------------------------------------------------------------
{-
  A module is a compilation unit which exports functions, types, classes, instances, and other
  modules. A module can only be defined in one file, though its exports may come from multiple
  sources. To make a Haskell file a module, just add a module declaration at the top.
-}

-- ### Simple example no. 1

  module CartProd where -- define the cartesian product operator and export it

    infixr 9 -*- -- right associative operator, precedence goes from 0 to 9, the strongest
    x -*- y = [(i,j) | i <- x, j <- y]

-- ### Simple example no. 2

  module Tree ( Tree(Leaf ,Branch), fringe ) where -- modulo si chiama Tree, e contiene il TYPE Tree e la funzione fringe
    data Tree a = Leaf a | Branch (Tree a) (Tree a)
    fringe :: Tree a -> [a]
      fringe (Leaf a) = [a]
      fringe (Branch a b) =  fringe a ++ fringe b -- tale funzione appiattisce l'albero e ne dá una rappresentazione grafica piatta

  module Main (main) where
    import Tree ( Tree(Leaf ,Branch) )
    main = print (Branch (Leaf ’a’) (Leaf ’b’))

-- ### Abstract Data Types

  {-
  modules provide the only way to build abstract data types (ADT) the characteristic feature of an ADT is that the representation
  type is hidden: all operations on the ADT are done at an abstract level which does not depend on the representation
  e.g. a suitable ADT for binary trees might include the following operations
  -}

    module  TreeADT (Tree , leaf , branch, cell,
            left , right , isLeaf)            
    where
    
    data Tree a = Leaf a | Branch (Tree a) (Tree a)
    
    createLeaf :: Tree a -> Leaf a
    createLeaf = Leaf
    
    createBranch :: Tree a -> Tree a -> Tree a
    createBranch = Branch
    
    cell :: Tree a -> a
    cell  (Leaf a)      = a

    left :: Tree a -> Tree a    
    left  (Branch l r)  = l

    right :: Tree a -> Tree a
    right (Branch l r)  = r
    
    isLeaf :: Tree a -> Bool
    isLeaf (Leaf _) = True
    isLeaf _        = False
    
    {- 
      in the export list the type name Tree appears without its constructors so the only way to build or take apart trees
      outside of the module is by using the various (abstract) operations the advantage of this information hiding is that 
      at a later time we could change the representation type without affecting users of the type.
    -}











--------------------------------------------------------------------------     
-- MONADIC TYPES
--------------------------------------------------------------------------     

-- ### Introduction

  class Monad m where  
      return :: a -> m a                 {-
                                            The first function that the Monad type class defines is return. 
                                            It's the same as pure, only with a different name. Its type is (Monad m) => a -> m a. 
                                            It takes a value and puts it in a minimal default context that still holds that value. 
                                            In other words, it takes something and wraps it in a monad. It always does the same thing as the pure function 
                                            from the Applicative type class, which means we're already acquainted with return. 
                                            We already used return when doing I/O. We used it to take a value and make a bogus I/O action that does nothing 
                                            but yield that value. For Maybe it takes a value and wraps it in a Just.

                                            Just a reminder: return is nothing like the return that's in most other languages. 
                                            It doesn't end function execution or anything, it just takes a normal value and puts it in a context.
                                          -}
    
      (>>=) :: m a -> (a -> m b) -> m b  {-
                                            The next function is >>=, or bind. 
                                            It's like function application, only instead of taking a normal value and feeding it to a normal function, 
                                            it takes a monadic value (that is, a value with a context) and feeds it to a function that takes a normal value but returns a monadic value.

                                            routine :: Maybe Pole  
                                            routine = do  
                                                start <- return (0,0)        -- same as return (0,0) >>=                (il risultato finale viene passato avanti >>=)
                                                first <- landLeft 2 start    -- same as return(0,0) >>= landLeft2 >>=   (il risultato finale viene passato avanti >>=)
                                          -}
      (>>) :: m a -> m b -> m b           {-
                                            Next up, we have >>. We won't pay too much attention to it for now because it comes with a default implementation 
                                            and we pretty much never implement it when making Monad instances.
                                          -}
      x >> y = x >>= \_ -> y              {-
                                            Here it is the default implementation!
                                            The default implementation is: DROP THE VALUE PASSED TO IT, SO NEVER SAVE THE PREVIOUS VALUE THAT IS PASSED TO IT.

                                            routine :: Maybe Pole  
                                            routine = do  
                                                start <- return (0,0)        -- same as return (0,0) >>=                (il risultato finale viene passato avanti >>=)
                                                first <- landLeft 2 start    -- same as return(0,0) >>= landLeft2 >>=   (il risultato finale viene passato avanti >>=)
                                                second <- landRight 2 first  -- sam as >>= landRight2 >>=               (il risultato finale viene passato avanti >>=)
                                                landLeft 1 second            -- >>= landLeft1 >>                        (il risultato finale viene BUTTATO E NON USATO >>) 
                                          -}
    
      fail :: String -> m a               -- Whenever we use fail, the computation stops with an error.
      fail msg = error msg                -- We never use it explicitly in our code. Instead, it's used by Haskell to enable failure in a special syntactic construct for monads
                                          -- that we'll meet later. We don't need to concern ourselves with fail too much for now.


-- ### Maybe

  {-
    Now that we know what the Monad type class looks like, let's take a look at how Maybe is an instance of Monad!
  -}

  -- New Implementation

    instance Monad Maybe where
        return :: a -> Maybe a        
        return a = Just a             -- the minimal context that the Maybe monad can give to a value is "the Just or Nothing" attribute

        >>= :: Maybe a -> (a -> Maybe b) -> Maybe b -- the bind operator here will work as expected: takes a Maybe monad, inspect it, and creates a new monad modified.
        Nothing >>= _ = Nothing -- takes a monad contains "Nothing": whathever will be the function to be binded, we will ignore it. 
        (Just a) >>= f = f a    -- takes a monad contains "Just a" and a function to be applied: it apply the function to the value inside Just, and create a new monad.

-- ### List

    instance Monad [] where
        return :: a -> [a]
        return x = [x]                -- the minimal context that the List monad can give to a value is "the fact of being inside a list", a list with only one value
         
        >>= :: [a] -> (a -> [b]) -> [b] -- the bind operator here will work as expected: takes a List monad, inspect it, and creates a new monad modified
        xs >>= f = concat (map f xs)    -- here we take a list xs, we map the function f to each element of xs, and return a flattened version of results 
                                        -- (that can be not flat and "nested" based onf what kind of f we'll use. e.g.:
                                        --     \x -> [x,-x] $ [3,4,5] = [[3,-3],[4,-4],[5,-5]]
                                        --  so we have to flat them all:
                                        --     concat (\x -> [x,-x] $ [3,4,5]) = [3,-3,4,-4,5,-5]
        fail _ = [] -- The empty list [] is pretty much the equivalent of Nothing, because it signifies the absence of a result.


-- ### State monad
            
    newtype State st a = State { runState :: st -> (st, a) } -- The state is defined as a TRANSITION FUNCTION that passes from a state to another one computing also a new value

    instance Monad (State state) where
      return aValue = State ( \state -> (state, aValue) )   -- definiamo e creiamo una funzione f che prende un parametro "state" e restituisce (crea) una coppia (stato, valore):
                                                            -- ció che restituisce, é una funzione identitaria perché siamo nella funzione return, quindi non ci dobbiamo muovere
                                                            -- dallo stato corrente, semplicemente restiuiamo il parametro aValue all'interno di una monade stato.
      
      -- let f state = (state, aValue) restituisce la stessa cosa di State(\state -> (state, aValue))

      (State oldStateFunction) >>= g = 
                      State (\oldState ->
                                          let 
                                              -- prima aggiorniamo il vecchio stato mantenendo il valore "val" intatto utilizzando la transition function solo sul vecchio stato
                                              (newState, oldVal) = oldStateFunction oldState  -- mi restituirá una coppia (newState, oldVal) che useremo piú in basso
                                              
                                              -- Quindi aggiorniamo il valore oldVal concatenando la nuova funzione del bind g, facendo andare vanti la computazione
                                              newVal = g oldVal  

                                              -- Adesso siamo pronti per creare un nuovo stato, lo stato con il valore di stato aggiornato e anche il valore della computazione
                                              (State newTransictionFunction) = newVal

                                              -- Per poterlo restituire, devo restituire la funzione che crea lo stato a partire da newState, ovvero la newTransictionFunction
                                          in newTransictionFunction newState -- La funzione che andiamo a restituire creerá uno stato a partire dal nuovo stato newstate, e se applicata
                                                                             -- ci restituirá anche il nuovo valore newVal)
                            )

--------------------------------------------------------------------------     
-- CUSTOM MONADIC TYPES
--------------------------------------------------------------------------

  --- ### Pole (is not a variant of the state monad: here there is no state. The value returned is the state but is not sapareted from it!)

    type Birds = Int  
    type Pole = (Birds,Birds)  

    landLeft :: Birds -> Pole -> Maybe Pole  
    landLeft n (left,right)                                      -- n birds fly to the left of the pole
        | abs ((left + n) - right) < 4 = Just (left + n, right)  -- the pole is balanced
        | otherwise                    = Nothing                 -- Pierre will fall!
      
    landRight :: Birds -> Pole -> Maybe Pole  
    landRight n (left,right)                                     -- n birds fly to the right of the pole
        | abs (left - (right + n)) < 4 = Just (left, right + n)  -- the pole is balanced
        | otherwise                    = Nothing                 -- Pierre will fall!


    banana :: Pole -> Maybe Pole 
    banana _ = Nothing                                           -- Pierre will fail for sure.


    return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2  -- Pierre will fail! (2,4) 4 > 3!

    -- same with monadic environemtn

    routine :: Maybe Pole  
    routine = do  
        start <- return (0,0)  
        first <- landRight 2 start  
        second <- landLeft 2 first  
        landRight 2 second  

  --- ### Stack No. 2 (Monadic version)

    import Control.Monad.State   -- reuse the State monad definition
      
    type Stack = [Int]  
      
    pop :: State Stack Int  
    pop = State (\(x:xs) -> (xs, x))
      
    push :: Int -> State Stack ()  
    push a = State(\xs -> (a:xs,()))

    -- Now let's try it

    stackManip :: State Stack Int  
    stackManip = do  
        push 3  
        a <- pop  
        pop  

  --- ### Counter

   {- Text:
     - Define a simple variant of the state monad, where the state is an integer
     - number, and in which every application of a bind increments such number.
     -}


    -- data definition
    data Inc a = Inc (Int -> (Int, a))

    -- monad definition
    instance Monad Inc where
      return value = Inc (\counter -> (counter, value))
      Inc transitionFunction >>= bindFunction  =  Inc (\oldCounter ->  let  
                                          (newCounter, oldValue) = transitionFunction oldCounter
                                          (Inc newValue) = bindFunction oldValue
                                       in  newValue (newCounter+1))
    pass :: Inc Int
    pass = return 0 -- restituisce una coppia (valore_stato_precedente,0)

              
    tryInc :: Inc Int
    tryInc = 
      do x <- return 1    -- restituisce una coppia (0,1), il cui valore 1 NON VIENE USATO nella prossima espressione e viene perso: lo stato é quello iniziale! (30)
        pass          -- prende in input lo stato precedente, gli aggiunge 1, e utilizza 0 come valore: (31,0)
        pass          -- prende in input lo stato precedente, gli aggiunge 1, e utilizza 0 come valore: (32,0)
        x <- return (x+1)   -- x adesso assume il valore contenuto nello precedente piú 1, in piú il valore dello stato aumenta: (33,1)
        pass          -- prende in input lo stato precedente, gli aggiunge 1, e utilizza 0 come valore: (34,2)
        pass          -- prende in input lo stato precedente, gli aggiunge 1, e utilizza 0 come valore: (35,2)
          return (x+1)      -- prende il valore contenuto nello stato precedente e gli aggiunge 1, inoltre aumenta lo stato di 1 (36,3)


    main = let Inc a = tryInc  -- lancia la valutazione degli stati
            in  a 30            -- definisce lo stato iniziale: 30



  --- ### Tree No.2 (Monadic version)

    -- Definition

    data Tree a = Leaf a | Branch (Tree a) (Tree a) 
                  deriving (Show,Eq)

    mapTreeM :: (a -> State state b) -> Tree a -> State state (Tree b)
    mapTreeM f (Leaf a) = 
         f a >>= (\b -> return (Leaf b))

    mapTreeM f (Branch lhs rhs) = do
         lhs' <- mapTreeM f lhs
         rhs' <- mapTreeM f rhs
         return (Branch lhs' rhs')

  -- Utility methods   

    getState :: State state state
    getState = State(\state -> (state, state))

    putState :: state -> State state ()
    putState new = State(\_ -> (new, ()))

    collectTree :: Tree b -> State [b] (Tree b)
    collectTree tree = mapTreeM collectList tree
             where collectList v = do
                    cur <- getState
                    putState ( [v])
                    return v
                    
    tree2list tree = let State f = (collectTree tree)
                         in f []                

  -- An instance of a tree       

    testTree = (Branch 
                  (Branch
                      (Leaf 'a') (Branch (Leaf 'b') (Leaf 'c')))
                  (Branch
                      (Leaf 'd') (Leaf 'e')))

--------------------------------------------------------------------------     
-- MONADIC FUNCTIONS
--------------------------------------------------------------------------     

-- ### Standard Monadic Functions
  
  {-
    To loop over a list with a monadic function, you would use mapM 
  -}
  mapM :: Monad m => (a -> m b) -> [a] -> m [b]
  mapM f = sequence . map f.

{-
    To loop over a list with a monadic function and forget about the result, you would use mapM_
  -}
  mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
  mapM_ f = sequence_ . map f.


  {-
    Evaluate each action in the sequence from left to right, and collect the results.
  -}

  sequence :: Monad m=>[m a]-> m [a]
  sequence  = foldr mcons (return [])
                  where mcons p q = do x <- p; xs <- q; return (x:xs)

  {-
    Evaluate each action in the sequence from left to right, and forget about the results.
  -}

  sequence _ :: Monad m => [m a] -> m()
  sequence _ xs = do sequence xs; return ()





-- ### Custom Monadic Functions
  
  -- The "unless" procedure

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

  -- The "getCSV" procedure

    {-
      Define a procedure getCSV that is used, in the Monad IO, to get a field in a Comma Separated Values
    -}    
                
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
            
            
  



    data Inc a = Inc (Int -> (Int, a))   -- data definition: this Monad contains the value that need to be computed, and A COUNTER (in this case, the state is the counter)

    instance Monad Inc where           -- Inc is a Monad!
    	return newInc = Inc (\aValue -> (theCounter, aValue))     -- The minimum context this monad wants to build is a pair (theCounter, aValue) i.e.
                                                                               -- a state "theCounter" (from the definition an INT that will count) 
                                                                               -- a value "aValue", that when we use result is unchanged. 
                                                                               -- In the bind, this will be the result of computation made on "aValue" passed to the monad

    	Inc aValue >>= f =                               -- Here we define the bind: we pass the monad "aValue" in order to be processed by the function f.
              Inc (
                    \aValue ->


                      (f aValue, (oldCounter + 1))                          -- This creates a new state monad where the new counter is the counter modified( here incremented by 1 )
            					 let 
                          (oldCounter, oldValue) = (oldStatus, aValue) -- new definitions
            					    Inc newValue = f oldValue
            					 in ((oldCounter + 1), newStatus)
                  )


    -- let's try it
    pass :: Inc Int
    pass = return 0

    esmm :: Inc Int
    esmm = do 
        x <- return 1
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




-- ### Greatest common divisor

  -- Classic definition
  gcdf x y | x == y = x
  gcdf x y | x < y = gcdf x (y-x)
  gcdf x y = gcdf (x-y) y


  -- Using monads...
  type ImpState = (Int, Int)

  getX, getY :: State ImpState Int
  getX = State(\(x,y) -> ((x,y), x))
  getY = State(\(x,y) -> ((x,y), y))

  putX, putY :: Int -> State ImpState ()
  putX x' = State (\(x,y) -> ((x', y), ()))
  putY y' = State (\(x,y) -> ((x, y'), ()))

  gcdST = do {
                  x <- getX; 
                  y <- getY;
                  (if x == y
                   then return x
                   else
                      if x < y
                      then do {putY (y-x); gcdST }
                      else do {putX (x-y); gcdST }
                   )
             } 

  runStateM :: State state a -> state -> a
  runStateM (State f) st = snd (f st)


  main :: IO()              
  main = print $ runStateM gcdST (21, 17)
                                  



















---------------------------------------------------------------
-- PARALLELISM
---------------------------------------------------------------

-- ### SEMIEXPLICIT

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

-- ### QUICKSORT

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


-- ### FIBONACCI PARALLELO

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









