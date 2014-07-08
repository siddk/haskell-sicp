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
