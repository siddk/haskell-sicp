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


