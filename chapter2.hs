-- Chapter 2 of A Structure and Interpretations of Computer Programs
-- This file contains problem statements, and haskell solutions for selected problems.

-- Notes on compiling the file: Like the Scheme workflow for SICP, this file does not need to
-- be compiled... rather, just import the function definitions into the Haskell interpreter (ghci)
-- to test your functions.

-- To do this, run [:l filename.hs] in the ghci shell in the directory housing your code.

-- Discontinuities between Haskell and Scheme at this point of SICP:
-- Scheme has a special pair type, which is defined by the cons operator. Haskell, on the other hand, has no specific dotted pair type, but rather has two compatible data types --> the tuple, and the list.
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


-- Exercise 2.17
-- Define a procedure last-pair that returns the list that contains only the last element of a given (nonempty) list:
-- (last-pair (list 23 72 149 34)) --> (34)
-- NOTE: Syntactic sugar in Haskell allows for multiple ways to address basic list functions. Whereas in Scheme, car and cdr and really the only list selectors, Haskell allows for functions head, tail, init, last, and variations thereof. For the continuity between SICP and Haskell, I will be sticking to the selectors head and tail, and their syntactic variants (x:xs) (head:tail).
lastPair :: [a] -> [a]
lastPair (x:[]) = [x]
lastPair (x:xs) = lastPair(xs)


-- Exercise 2.18
-- Define a procedure reverse that takes a list as argument and returns a list of the same elements in reverse order
-- NOTE: Reverse is predefined in the Standard Haskell Prelude, so I use the standard ' notation to denote a function variation.
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs $ ++ [x]
