{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
% CS 340: Programming Paradigms and Patterns
% Lect 06 - Recursion
% Michael Lee

\begin{code}
module Lect06 where
import Data.List (nub)
import Debug.Trace
import qualified Data.Set as Set
\end{code}

Recursion
=========

Agenda:

  - Some common patterns of recursion:
     A. Iteration & Reduction
     B. Filtering
     C. Accumulation
     D. Combinations & Permutations
     E. Divide & Conquer


A. Iteration & Reduction (Processing elements one by one)

\begin{code}
-- sum up the elements of a list
sum' :: Num a => [a] -> a
sum' [] = 0 
sum' (x:xs) = x + sum' xs 

-- a classic!
factorial :: Integer -> Integer
factorial 0 = 1 
factorial 1 = 1 
factorial n = n * factorial (n-1)
\end{code}


B. Filtering (Selective iteration/reduction)

\begin{code}
-- sum only the positive numbers in a list
sumPositives :: Integral a => [a] -> a
sumPositives [] = 0
sumPositives (x:xs) | x > 0     = x + sumPositives xs 
                    | otherwise = sumPositives xs 
-- sumPositives (x:xs) = (if x > 0 then x else 0) + sumPositives xs
-- line 49 same as lines 47/48 but with if else statement

-- palindroms are strings that read the same forwards as backwards
palindromes :: [String] -> [String]
palindromes [] = [] 
palindromes (s:ss) | s == reverse s = s : palindromes ss
                   | otherwise      = palindromes ss
--palindromes (s:ss) = (if s == reverse s then (s:) else id) (palindromes ss)
-- line 57 is the same as lines 55/56 but with if else and id function
\end{code}


C. Accumulation (Computing/Passing information "down" while recursing)

\begin{code}
-- count even numbers
countEvens :: Integral a => [a] -> Int
countEvens [] = 0 
countEvens (n:ns) | n `mod` 2 == 0 = 1 + countEvens ns 
                  | otherwise      = countEvens ns 
--countEvens ns = helper ns 0
--  where helper [] acc = acc
--        helper (m:ms) acc = helper ms (if m `mod` 2 == 0 then acc + 1 else acc)
-- Lines 71/72/73 same as 68/69/70 written diff

-- reverse a list
reverse' :: [a] -> [a]
--reverse' [] = [] 
--reverse' (x:xs) = reverse' xs ++ [x]
-- Lets write reverse instead using accumulation pattern
reverse' xs = helper xs []
  where helper [] acc     = acc 
        helper (y:ys) acc = helper ys (y:acc)
\end{code}

helper (1:2:3:[]) []
= helper (2:3:[]) (1:[]) 
= helper (3:[]) (2:1:[])
= helper ([]) (3:2:1:[])
= (3:2:1:[]) 

D. Combinations & Permutations (Essential combinatorics)

\begin{code}
-- generate all combinations of elements in a list (order doesn't matter)
combinations :: [a] -> [[a]]
combinations = undefined

-- the knapsack problem: given a list of items (value,weight) and a weight 
-- capacity, find the maximum value that can be carried
knapsack :: (Ord a, Num a) => a -> [(a,a)] -> a
knapsack = undefined

-- generate all permutations of elements in a list (order matters)
permutations :: [a] -> [[a]]
permutations = undefined

-- generate all palindromes from a given string (use `nub` to remove dups)
allPalindromes :: String -> [String]
allPalindromes = undefined
\end{code}


E. Divide & Conquer (Break a problem into smaller ones of the same structure)

\begin{code}
-- a classic!
fib :: Integral a => a -> a
fib = undefined

-- sort by splitting the list in half and merging the sorted halves
mergesort :: Ord a => [a] -> [a]
mergesort = undefined
\end{code}


F. Generative recursion (Generates new subproblems (in size/structure))

\begin{code}
newtonSqrt :: Double -> Double -> Double
newtonSqrt = undefined
\end{code}
