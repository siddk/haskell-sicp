-- Chapter 1 of A Structure and Interpretations of Computer Programs
-- This file contains problem statements, and haskell solutions for selected problems.

-- Notes on compiling the file: Like the Scheme workflow for SICP, this file does not need to
-- be compiled... rather, just import the function definitions into the Haskell interpreter (ghci)
-- to test your functions.
--
-- To do this, run [:l filename.hs] in the ghci shell in the directory housing your code.

-- Exercise 1.3.
-- Define a procedure that takes three numbers as arguments and returns the sum
-- of the squares of the two larger numbers.
sumBig2 :: (Ord a, Num a) => a -> a -> a -> a
sumBig2 x y z
    | x == (larger x y) = sumSquares x (larger y z)
    | otherwise = sumSquares y (larger x z)

larger :: (Ord a, Num a) => a -> a -> a
larger x y
    | x > y = x
    | otherwise = y

sumSquares :: (Ord a, Num a) => a -> a -> a
sumSquares x y = (x^2) + (y^2)


-- Exercise 1.5 (normal vs. applicative order)
-- Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using applicative-order evaluation or normal-order evaluation. He defines the following two procedures:
-- (define (p) (p))
-- (define (test x y)
--  (if (= x 0)
--      0
--      y))
-- Then he evaluates the expression
--(test 0 (p))
-- What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What behavior will he observe with an interpreter that uses normal-order evaluation? Explain your answer. (Assume that the evaluation rule for the special form if is the same whether the interpreter is using normal or applicative order: The predicate expression is evaluated first, and the result determines whether to evaluate the consequent or the alternative expression.)
--------------------------------------------------------------------------------
-- With applicative order evaluation, all the conditionals are evaluated first, which means the (p) is evaluated unto itself, forcing an infinite loop.
-- With normal order evaluation, the (p) term never gets evaluated, and the program runs.


-- Exercise 1.6
-- Alyssa P. Hacker doesn't see why if needs to be provided as a special form. ``Why can't I just define it as an ordinary procedure in terms of cond?'' she asks. Alyssa's friend Eva Lu Ator claims this can indeed be done, and she defines a new version of if:
-- (define (new-if predicate then-clause else-clause)
--  (cond (predicate then-clause)
--        (else else-clause)))
-- Eva demonstrates the program for Alyssa:
-- (new-if (= 2 3) 0 5)
-- 5
-- (new-if (= 1 1) 0 5)
-- 0
-- Delighted, Alyssa uses new-if to rewrite the square-root program:
-- (define (sqrt-iter guess x)
--   (new-if (good-enough? guess x)
--           guess
--           (sqrt-iter (improve guess x)
--                      x)))
-- What happens when Alyssa attempts to use this to compute square roots? Explain.
---------------------------------------------------------------------------------
-- (SCHEME) new-if is a function (not a special form), and as Scheme uses applicative order evaluation, the arguments of the function are always evaluated first, so the second clause will continue to be evaluated, resulting in a loop.
-- (HASKELL) Haskell uses lazy evaluation, and as such, the function arguments will not be evaluated unless needed, so the program will run successfully.


-- Exercise 1.8
-- Newton's method for cube roots is based on the fact that if y is an approximation to the cube root of x, then a better approximation is given by the value
-- ((x/y^2)+2y) / 3
-- Use this formula to implement a cube-root procedure analogous to the square-root procedure. (In section 1.3.4 we will see how to implement Newton's method in general as an abstraction of these square-root and cube-root procedures.)
cubeRootIter :: (Num a, Ord a, Fractional a) => a -> a -> a
cubeRootIter guess x = if (goodEnough guess x)
                       then guess
                       else cubeRootIter (improve guess x) x

goodEnough :: (Num a, Ord a, Fractional a) => a -> a -> Bool
goodEnough guess x = (abs ((guess * guess * guess) - x)) < 0.001

improve :: (Num a, Ord a, Fractional a) => a -> a -> a
improve guess x = ((x / (guess * guess)) + (2 * guess)) / 3


-- Exercise 1.16
--  Design a procedure that evolves an iterative exponentiation process that uses successive squaring and uses a logarithmic number of steps, as does fast-expt. (Hint: Using the observation that (bn/2)2 = (b2)n/2, keep, along with the exponent n and the base b, an additional state variable a, and define the state transformation in such a way that the product a bn is unchanged from state to state. At the beginning of the process a is taken to be 1, and the answer is given by the value of a at the end of the process. In general, the technique of defining an invariant quantity that remains unchanged from state to state is a powerful way to think about the design of iterative algorithms.)
fastExpt :: (Num a, Ord a) => a -> Int -> a
fastExpt base n = fastExptIter 1 base n

fastExptIter :: (Num a, Ord a) => a -> a -> Int -> a
fastExptIter cur base 0 = cur
fastExptIter cur base n = if even n
                          then fastExptIter cur (base * base) (n `div` 2)
                          else fastExptIter (cur * base) base (n - 1)


-- Exercise 1.30
-- The sum procedure above generates a linear recursion. The procedure can be rewritten so that the sum is performed iteratively. Show how to do this by filling in the missing expressions in the following definition:
-- (define (sum term a next b)
--   (define (iter a result)
--     (if <??>
--         <??>
--         (iter <??> <??>)))
--   (iter <??> <??>))
iterSum :: (Num a, Ord a) => (a -> a) -> a -> (a -> a) -> a -> a
iterSum term a next b = iter a 0
    where iter a result = if a > b
                          then result
                          else iter (next a) (result + (term a))


-- Exercise 1.31
-- a.  The sum procedure is only the simplest of a vast number of similar abstractions that can be captured as higher-order procedures. Write an analogous procedure called product that returns the product of the values of a function at points over a given range. Show how to define factorial in terms of product.
iterProduct :: (Num a, Ord a) => (a -> a) -> a -> (a -> a) -> a -> a
iterProduct term a next b = iter a 1
    where iter a result = if a > b
                          then result
                          else iter (next a) (result * (term a))

factorialProduct :: (Num a, Ord a) => a -> a
factorialProduct n = iterProduct (\x -> x * 1) 1 (\x -> x + 1) n

-- b.  If your product procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.
recursiveProduct :: (Num a, Ord a) => (a -> a) -> a -> (a -> a) -> a -> a
recursiveProduct term a next b = if a > b
                                 then 1
                                 else (term a) * (recursiveProduct term (next a) next b)


-- Exercise 1.32
-- Show that sum and product (exercise 1.31) are both special cases of a still more general notion called accumulate that combines a collection of terms, using some general accumulation function:
-- (accumulate combiner null-value term a next b)
-- Accumulate takes as arguments the same term and range specifications as sum and product, together with a combiner procedure (of two arguments) that specifies how the current term is to be combined with the accumulation of the preceding terms and a null-value that specifies what base value to use when the terms run out. Write accumulate and show how sum and product can both be defined as simple calls to accumulate.

-- (HASKELL) It's is important to note at this time that haskell has two functions foldr and foldl that will essentially apply accumulate over a LIST, not a range of two values. As such, we will define accumulate as it appears here in haskell as follows:
accumulate :: (Ord a) => (a -> a -> a) -> a -> (a -> a) -> a -> (a -> a) -> a -> a
accumulate combiner null_value term x next n = iter x null_value
    where iter x result = if x > n
                          then result
                          else iter (next x) (combiner (term x) result)


-- Exercise 1.33
-- You can obtain an even more general version of accumulate (exercise 1.32) by introducing the notion of a filter on the terms to be combined. That is, combine only those terms derived from values in the range that satisfy a specified condition. The resulting filtered-accumulate abstraction takes the same arguments as accumulate, together with an additional predicate of one argument that specifies the filter. Write filtered-accumulate as a procedure. Show how to express the following using filtered-accumulate:
-- a. the sum of the squares of the odd numbers in the interval a to b
filteredAccumulate :: Ord a => (a -> Bool) -> (a -> a -> a) -> a -> (a -> a) -> a -> (a -> a) -> a -> a
filteredAccumulate predicate combiner null_val term x next n = iter x null_val
    where iter x result
            | x > n = result
            | predicate x = iter (next x) (combiner (term x) result)
            | otherwise = iter (next x) result

sumOddSquares :: (Integral a) => a -> a -> a
sumOddSquares a b = filteredAccumulate odd (+) 0 (\x -> x * x) a (\x -> x + 1) b
