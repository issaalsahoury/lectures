% CS 340: Programming Paradigms and Patterns
% Lect 04 - Lists
% Michael Lee

> module Lect04 where
> import Data.Char
> import Debug.Trace
  
Lists
=====

Agenda:
  - The List type
  - Syntactic sugar
  - Constructing lists
  - List comprehensions
  - Common list functions
  - List processing functions
    - Pattern matching
    - Structural recursion


The List type
-------------

Haskell's built-in list type might be defined something like this:

    [a] = [] | a : [a]

<<<<<<< Updated upstream
<<<<<<< Updated upstream
- How should we interpret this?
=======
=======
>>>>>>> Stashed changes
    'a' here stands for a type variable

Read as: a list of type 'a' ([a]) is either an empty list ([]) or 
         value of type 'a' followed by ':' and a list of type 'a' ([a]).
>>>>>>> Stashed changes

- What are some ramifications of this definition?

- Try using `[]` and `:` to build some lists.

    [Int] = [] | Int : [Int]

    [3] = 3:[]
    [4,3] = 4:(3:[])
    [10, 8, 6, 4, 3] = 10(8: (6:( 4: (3:[]))))

    
    [[Int]] = [] | [Int] : [[Int]]

    (4:(3:[])) : ((3:[]:[]))



    [Int] = [] | Int : [Int]

    [3] = 3:[]
    [4,3] = 4:(3:[])
    [10, 8, 6, 4, 3] = 10(8: (6:( 4: (3:[]))))

    
    [[Int]] = [] | [Int] : [[Int]]

    (4:(3:[])) : ((3:[]:[]))




Syntactic sugar
---------------

Instead of constructing lists with `:`, there is syntactic sugar:

- basic itemized lists: [1,2,3,4,5]

- strings, aka lists of Chars: "hello world"

- arithmetic sequences: [i..j], [i,j..k]

- infinite sequences: [i..], [i,j..]


Constructing lists
------------------

Functions that construct lists typically:

  - operate recursively

  - add just one element with `:`, when necessary, and recurse to construct 
    the rest of the list

  - terminate the list in the base case (no longer call yourself) with `[]`


E.g., implement the following list construction functions:

> replicate' :: Int -> a -> [a]
> replicate' 0 _ = []
> replicate' 1 x = [x]
> replicate' n x = x : replicate' (n-1) x -- this takes 2 arguments n and x.

> enumFromTo' :: (Ord a, Enum a) => a -> a -> [a] --Ord means we can sort. Orderable.
> enumFromTo' start stop -- Enum means you can get the predecessor & successor.
>   | start > stop = [] 
>   | otherwise = start : enumFromTo' (succ start) stop
>
> -- and now for some infinite lists
>
> ones :: [Int]
> ones = 1 : ones 
> 
> repeat' :: a -> [a]
> repeat' x = x : repeat' x
>
> enumFrom' :: Enum a => a -> [a]
<<<<<<< Updated upstream
<<<<<<< Updated upstream
> enumFrom' = undefined
=======
> enumFrom' x = x : enumFrom' (succ x)
> -- head gives us the first value of a list so head [1,2,3] is 1, etc. 
>>>>>>> Stashed changes
=======
> enumFrom' x = x : enumFrom' (succ x)
> -- head gives us the first value of a list so head [1,2,3] is 1, etc. 
>>>>>>> Stashed changes

Note: use `take` to limit the number of values drawn from an infinite list


List comprehensions
-------------------

Syntax:

  [ Expression | Generator, ... , Predicate, ... ]

E.g.,

> evens = [ 2*x | x <- [1..10]]
>
> evens' = [x | x <- [1..10], x `mod` 2 == 0]
>
> integerRightTriangles p = [(a,b,c) | a <- [1..p], 
>                                      b <- [a..(p-a)],
>                                      let c = p-(a+b),
>                                      a^2 + b^2 == c^2]

E.g., try implementing:

> factors :: Integral a => a -> [a] --factors n = [i | i <- [1..n], n `mod` i == 0] same as below

> factors n = collect 1
>    where divisible i = n `mod` i == 0
>          collect i | i == n = [i]
>                     | divisible i = i : collect(i+1)
>                     | otherwise = collect(i+1)
>
>
> cartesianProduct :: [a] -> [b] -> [(a,b)]
> cartesianProduct xs xy = [ (x,y) | x <- xs, y <- xy ]
>
> concat' :: [[a]] -> [a]
> concat' ls = [ x | l <- ls, x <- l ]


Common list functions
---------------------

The "Prelude" module defines many useful list functions (some of which we 
implemented above). They include:

  - Basic operations:

    head :: [a] -> a
    tail :: [a] -> [a]
    null :: [a] -> Bool
    length :: [a] -> Int
    last :: [a] -> a
    (++) :: [a] -> [a] -> [a]
    (!!) :: [a] -> Int -> a

  - Building lists:

    repeat :: a -> [a]
    replicate :: Int -> a -> [a]
    cycle :: [a] -> [a]

  - Lists -> Lists:

    concat :: [[a]] -> [a]
    reverse :: [a] -> [a]
    zip :: [a] -> [b] -> [(a,b)]

  - Extracting sublists:

    take :: Int -> [a] -> [a]
    drop :: Int -> [a] -> [a]
    splitAt :: Int -> [a] -> ([a], [a])
    break :: (a -> Bool) -> [a] -> ([a], [a])

  - Class specific:

    elem :: Eq a => a -> [a] -> Bool
    maximum :: Ord a => [a] -> a
    minimum :: Ord a => [a] -> a
    sum :: Num a => [a] -> a
    product :: Num a => [a] -> a
    lines :: String -> [String]
    words :: String -> [String]

Note: many of these functions operate on a type class that includes lists and
      other recursive data types (We'll see how this works later.)


List processing functions
-------------------------

-- Pattern matching

`[]` and `:` can be used to pattern match against lists (we'll see that all
value constructors can be used for pattern matching). 

E.g., implement:

> head' :: [a] -> a
> head' [] = error "Empty List"
> head' (x:_) = x --theres an element x in front of some remaining elements
>
> tail' :: [a] -> [a]
> tail' [] = error "Empty List"
> tail' (_:xs) = xs 
> 
> null' :: [a] -> Bool
> null' [] = True
> null' _ = False


-- Structural recursion

Structural recursion describes a pattern for writing functions that process recursively defined data types (like the list). 

Generally, a structurally recursive function will:

  1. start by determining how the value was constructed

  2. if the value is not a recursive instance of the data type, (e.g., `[]`) 
     just process it directly

  3. if the value is a recursive instance of the data type, "deconstruct" it to 
     process one value, then recurse to process the rest of the values.

We will always have a base case with structural recursion. 

Pattern matching in Haskell helps with both (1) and (3).

E.g., to compute the length of a list:

> length' :: [a] -> Int
> length' [] = 0 -- shows you that we have a base case
> length' (_:xs) = 1 + length' xs -- pattern match here

E.g., implement more built-in functions:

> last' :: [a] -> a
> last' [] = error "empty list"
> last' (x:[]) = x -- it will keep recursing past the last element -- last' [x] = x
> last' (_:xs) = last' xs -- given list of atleast one element, return xs 
>
>
> take' :: Int -> [a] -> [a]
> take' _ [] = []
> take' 0 _ = []
> take' n (x:xs) = x : take' (n-1) xs 
>
>
> drop' :: Int -> [a] -> [a]
> drop' _ [] = []
> drop' 0 xs = xs 
> drop' n (x:xs) = drop' (n-1) xs 
>
>
> (!!!) :: [a] -> Int -> a 
> (!!!) (x:_) 0  = x  -- can also do:   (x:_) !!! 0 = x (all preference)
> (!!!) [] i     = error "index too large" -- [] !!! i = error 
> (!!!) (_:xs) i = xs !!! (i-1) -- (_:xs) !!! i = xs !!! (i-1)
>
>
> splitAt' :: Int -> [a] -> ([a], [a])
> splitAt' 0 xs = ([], xs)
> splitAt' _ [] = ([], [])
> splitAt' n (x:xs) = let (ys, zs) = splitAt' (n-1) xs
>                     in (x:ys, zs)
>
> break' :: (a -> Bool) -> [a] -> ([a], [a]) -- function that returns bool given value = predicate
> break' _ [] = ([], [])                    -- need to use guards to deal with predicates
> break' p l@(x:xs) | p x = ([], l) -- can remove l@ and put x:xs near empty set
>                 | otherwise = let (ys, zs) = break' p xs 
>                               in (x:ys, zs)
>
>
> words' :: String -> [String]
> words' "" = []
> words' (c:cs) | isSpace c = words' cs 
>               | otherwise = let (w, ws) = break' isSpace (c:cs)
>                             in w : words' ws 


E.g., the Caesar cipher is an encryption scheme that takes a plain text input
string P and a shift value N, and produces an encrypted version of the string
by replacing each letter in P with one N letters away in the alphabet (wrapping
around, if needed).

For the string "HELLO WORLD" and a shift value of 5:

  Plain:      H E L L O  W O R L D
  Encrypted:  M J Q Q T  B T W Q I

To implement the Caesar cipher, we need to be able to convert characters from/to
their corresponding ASCII codes. `ord`/`chr` do this. `isLetter` can be used to
determine if a character is a letter. We'll convert all letters to uppercase
for simplicity with `toUpper`.

> caesar :: Int -> String -> String
> caesar = undefined
