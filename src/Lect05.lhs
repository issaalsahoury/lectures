% CS 340: Programming Paradigms and Patterns
% Lect 05 - Testing
% Michael Lee

\begin{code}
module Lect05 where
import Control.Exception
import Test.Hspec
import Test.QuickCheck
\end{code}

Testing
=======

Agenda:
  - What is testing?
  - Approaches to testing & verification
  - Hspec testing framework
  - Example-based tests with Hspec
  - Property-based tests with QuickCheck
  - Test coverage


What is testing?
----------------

A *test* verifies that some aspect of a system works to specification.

- What are some important features of test frameworks? 

    - test our code down to lower level functions (rather than high level specs)

    - test individual functions (units) rather than the entire program 

    - test that modules/functions, when glued together, work correctly (integration test)

    - validate that the original specification/requirements are correct/workable 

    - ensure that we don't break things when we add new features (regression testing) 

    - performance analysis (for optimization) -- identifying bottlenecks 

    - Simplify test discovery, executing, and reporting 

    - determine (1) test coverage and (2) code lint (aka "dead code") 


Testing tools can help:

  - simplify test specification, discovery, execution, and reporting

  - ensure that code changes don't break existing functionality (no regressions)

  - determine code coverage (how much of the codebase is actually run)

  - eliminate code "lint" (aka "dead code")


Approaches to testing & verification
------------------------------------

General strategy: Test-Driven Development (TDD)

  - Write tests *first*, ensure they fail, then write code to get them to pass

  - After all tests pass, any future code refactoring requires re-running tests

  1. Write tests (specification) first 
  2. Ensure that all tests fail 
  3. Write your code, running tests on each commit 
  4. Once all tests pass, you are done! 

  - What types of test / verification exist? 

    - Unit tests check that units of code (typically, functions, clases, etc) 
      work according to some specification 

      - example based test: specify expected outputs/results for given 
        input/arguments; sometimes, we also need to specify "state" 

      - Property-based test: declares high level *property* (aka invariant) 
        that must hold true for all inputs and/or state. This specific examples 
        used for verification are automatically generated by the framework. 

      - Integration tests (make sure that when units are combined, the resulting
        system works as specified)

      - Formal verification (proofs) 





But how to write tests? (How to verify correctness?)

  - *Static tests* are carried out by a compiler, which checks for syntax and
     type related errors. We write *type signatures* to help the compiler.

  - *Unit tests* check that "units" of code (e.g., functions, classes) work as 
    expected. Their specification/execution is facilitated by test frameworks.

      - *Example-based tests* explicitly declare the expected results (e.g.,
        return value, output, exception) for different inputs and/or state.
      
      - *Property-based tests* declare high-level "properties" (aka invariants) 
        that must hold true for all inputs and/or state. Specific cases are 
        automatically generated and checked.
              
  - *Formal verification* may be done at a higher level of abstraction. It is
    typically done by a theorem prover, which checks for logical errors by
    proving that the program satisfies a set of logical properties.


Hspec testing framework (a domain specific language - DSL)
-----------------------

Hspec gives us a way to specify tests in a human-legible way:

\begin{code}
someSpec :: Spec
someSpec = 
  describe "someFunc" $ do
    it "fulfills some expectation ..." $
      pendingWith "Need to flesh out this test"
    it "fulfills some other expectation ..." $
      pending
\end{code}

  - Run a `Spec` using `hspec`.

  - Hspec supports both unit tests and property-based tests

  - `stack test` will run "test/Spec.hs", which will automatically discover
    all "*Spec.hs" files in the "test" directory and run their "spec" functions

    - E.g., all tests for this lecture are in "test/Lect05Spec.hs"


Example-based tests with Hspec
------------------------------

Hspec provides various functions for creating `Expectations`:

  - `shouldBe` / `shouldNotBe`
  - `shouldSatisfy` / `shouldNotSatisfy`
  - `shouldMatchList` / `shouldNotMatchList`
  - `shouldThrow` / `shouldNotThrow`

E.g., let's write a specification for `c2k`, `c2f`, `f2c`:

\begin{code}
c2k :: (Ord a, Floating a) => a -> a
c2k c | c >= 0 = c + 273.15
      | otherwise = error "Temperature below absolute zero"


c2f :: Floating a => a -> a
c2f c = c * 9/5 + 32


f2c :: Floating a => a -> a
f2c f = (f - 32) * 5/9


celsiusConversionSpec :: Spec
celsiusConversionSpec = 
  describe "Celsius conversions" $ do
    describe "c2k" $ do
      it "works for known examples" $ do
        c2k 0 `shouldBe` 273.15
        c2k 100 `shouldBe` 373.15
        c2k 200 `shouldBe` 473.15 
      it "fails for sub-abs-zero temperatures" $ do
        evaluate (c2k (-300)) `shouldThrow` anyException
    describe "c2f" $ do
      it "works for known examples" $ do
        c2f 0  `shouldBe` 32
        c2f 100  `shouldBe` 212 
        c2f 500.1 `shouldSatisfy` (=~= 932.18)
    describe "f2c" $ do
      it "works for known examples" $ do
        f2c 100 `shouldSatisfy` (=~= 37.778)

infix 4 =~= 
(=~=) :: (Floating a, Ord a) => a -> a -> Bool
x =~= y = abs (x - y) < 0.0001
\end{code}


E.g., let's write a specification for `quadRoots`

\begin{code}
quadRoots :: (Floating a, Ord a) => a -> a -> a -> (a, a)
quadRoots a b c 
    | disc >= 0 = ((-b + sqrt_d) / (2*a), (-b - sqrt_d) / (2*a))
    | otherwise = error "No real roots"
  where disc   = b^2 - 4*a*c
        sqrt_d = sqrt disc


quadRootsSpec :: Spec
quadRootsSpec = 
  describe "quadRoots" $ do
    it "works for known examples" $ do
      pending
    it "fails when non-real roots exist" $ do
      pending
\end{code}


Discussion: what are some problems / shortcomings of example-based testing?


Property-based tests with QuickCheck
------------------------------------

QuickCheck is the original property-based testing framework. To use it, we
specify properties for the unit being tested, and QuickCheck will automatically
generate test cases to check that the property holds.

A property is function that takes test inputs and returns `Bool` or `Property`. 

  - properties must be monomorphic (i.e., they can't have type variables), as
    QuickCheck needs concrete types to create random values

  - "generators" produce random test cases, and can be customized

  - if QuickCheck can falsify a property (i.e., prove that it doesn't hold), it
    tries to "shrink" the test cases to give us a minimal counterexample


E.g., write a property to test that `c2f` and `f2c` are inverses:


\begin{code}
prop_c2f2c :: Double -> Bool
prop_c2f2c c = f2c (c2f c) =~= c 

cTemp :: Gen Double
cTemp = choose (-273.15, 1000)

prop_c2f2c' :: Property 
prop_c2f2c' = forAll cTemp prop_c2f2c
\end{code}


E.g., write a property to test `mySum` using `sum` as a reference implementation (what happens if you break `mySum`?):

\begin{code}
mySum :: (Eq a, Num a) => [a] -> a
mySum [] = 0
--mySum (x:xs) = x + mySum xs
mySum (7:8:xs) = 7 + 9 + mySum xs
mySum (x:xs) = x + mySum xs 

prop_sum :: [Integer] -> Bool
prop_sum xs = mySum xs == sum xs 
\end{code}


E.g., try writing properties to test distributivity of multiplication over
addition and commutativity of addition:

\begin{code}
prop_distMultOverAdd :: Integer -> [Integer] -> Bool
prop_distMultOverAdd n xs = mySum[n*x | x <- xs] == n * mySum xs


prop_commAdd :: [Integer] -> Property
prop_commAdd xs = forAll (shuffle xs) (\ys -> mySum ys == mySum xs)
\end{code}


E.g., write a property to test that `quadRoots` works correctly for perfect
squares and factorable quadratic equations:

\begin{code}
prop_perfSquare :: Double -> Bool
prop_perfSquare = undefined


prop_solvesFactored :: Double -> Double -> Bool
prop_solvesFactored = undefined
\end{code}


E.g., define a `Spec` combining property-based and unit tests:

\begin{code}
quadRootsSpec' :: Spec
quadRootsSpec' = 
  describe "quadRoots" $ do
    it "works for known examples" $ do
      pending
    it "fails when non-real roots exist" $ do
      pending
    it "works correctly with perfect squares" $ 
      pending
    it "works correctly with factorable quadratic equations" $ 
      pending
\end{code}


Test coverage
-------------

How much of our code are we actually testing? 

  - are there functions we're never calling?
  
  - are there patterns/guards/branches we're never matching/taking?

  - are there unreachable sections of code?

`stack test --coverage` generates a coverage report for all modules tested.

100 percent test coverage is a noble goal!
