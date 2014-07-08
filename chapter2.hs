-- Chapter 2 of A Structure and Interpretations of Computer Programs
-- This file contains problem statements, and haskell solutions for selected problems.

-- Notes on compiling the file: Like the Scheme workflow for SICP, this file does not need to
-- be compiled... rather, just import the function definitions into the Haskell interpreter (ghci)
-- to test your functions.

-- To do this, run [:l filename.hs] in the ghci shell in the directory housing your code.

-- Discontinuities between Haskell and Scheme at this point of SICP:
-- Scheme has a special pair type, which is defined by the cons operator. Haskell, on the other hand, has no specific pair type, but rather has two compatible data types --> the tuple, and the list.
-- In the following series of problems, I will do the best I can to use the corresponding data type. Note that the Tuple type can often be replaced by the List type, while the opposite may not necessarily be true.

-- Exercise 2.1
-- Define a better version of make-rat that handles both positive and negative arguments. Make-rat should normalize the sign so that if the rational number is positive, both the numerator and denominator are positive, and if the rational number is negative, only the numerator is negative.
makeRat :: (Integral a) => a -> a -> (a, a)
makeRat num den
    | negative num && negative den = ((-1) * num, (-1) * den)
    | negative num = (num, den)
    | negative den = (num * (-1), den * (-1))
    | otherwise = (num, den)

negative :: (Integral a) => a -> Bool
negative x = x < 0


-- Exercise 2.7
-- Alyssa's program is incomplete because she has not specified the implementation of the interval abstraction. Here is a definition of the interval constructor:
-- (define (make-interval a b) (cons a b))
-- Define selectors upper-bound and lower-bound to complete the implementation.
makeInterval :: (Integral a) => a -> a -> (a, a)
makeInterval lower upper = (lower, upper)

lowerBound :: (Integral a) => (a, a) -> a
lowerBound interval = fst interval

upperBound :: (Integral a) => (a, a) -> a
upperBound interval = snd interval


-- Exercise 2.8
-- Using reasoning analogous to Alyssa's, describe how the difference of two intervals may be computed. Define a corresponding subtraction procedure, called sub-interval.
addInterval :: (Integral a) => (a, a) -> (a, a) -> (a, a)
addInterval x y = makeInterval ((lowerBound x) + (lowerBound y)) ((upperBound x) + (upperBound y))

subInterval :: (Integral a) => (a, a) -> (a, a) -> (a, a)
subInterval x y = addInterval x (makeInterval (-1 * (upperBound y)) (-1 * (lowerBound y)))


-- Neato Problem
-- This problem was given out at the 2008 Santa Clara Valley Mathematics Field Day competition.
-- A triangular number, Tn = 1+2+3+ ... + n. There are some triangular numbers which are the products of three consecutive positive integers. For example, T3 = 1+2+3 = 1*2*3. Also, T22736 = 1+2+3+...+22736 = 636*637*638 = 258474216. NOTE: 258474216 is the largest of these numbers.
-- It turns out that there are exactly six triangular numbers which are the products of three integers.
-- Write a program that fills in the following table where TFTN# represents the index of the "Three Factors of Triangular Numbers." Your function should return a list containing six lists of three elements. It should be of the form: ((3 1 6) ... (22736 636 258474216)) where the first number in each triplet is the n in 1+2+...+n, the second number is the FIRST number in x(x+1)(x+2), and the last number is the triangular number.
triNum = triIter [] 1 3
    where triIter list x n
            | x > 636 = list
            | sum [1..n] == product [x, x+1, x+2] = triIter (list ++ [(n, x, sum [1..n])]) x (n + 1)
            | otherwise = if x > n
                          then triIter list x (n + 1)
                          else triIter list (x + 1) n