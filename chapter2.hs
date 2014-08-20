-- Chapter 2 of A Structure and Interpretations of Computer Programs
-- This file contains problem statements, and haskell solutions for selected problems.

-- Notes on compiling the file: Like the Scheme workflow for SICP, this file does not need to
-- be compiled... rather, just import the function definitions into the Haskell interpreter (ghci)
-- to test your functions.

-- To do this, run [:l filename.hs] in the ghci shell in the directory housing your code.

-- Discontinuities between Haskell and Scheme at this point of SICP:
-- Scheme has a special pair type, which is defined by the cons operator. Haskell, on the
-- other hand, has no specific dotted pair type, but rather has two compatible data types
-- the tuple, and the list.
-- In the following series of problems, I will do the best I can to use the corresponding
-- data type. Note that the Tuple type can often be replaced by the List type, while the
-- opposite may not necessarily be true.
-- Other discontinuities will be addressed as they come up.

-- Exercise 2.1
-- Define a better version of make-rat that handles both positive and negative arguments.
-- Make-rat should normalize the sign so that if the rational number is positive, both the
-- numerator and denominator are positive, and if the rational number is negative, only the
-- numerator is negative.
makeRat :: (Integral a) => a -> a -> (a, a)
makeRat num den
    | negative num && negative den = ((-1) * num, (-1) * den)
    | negative num = (num, den)
    | negative den = (num * (-1), den * (-1))
    | otherwise = (num, den)

negative :: (Integral a) => a -> Bool
negative x = x < 0


-- Exercise 2.7
-- Alyssa's program is incomplete because she has not specified the implementation of the
-- interval abstraction. Here is a definition of the interval constructor:
-- (define (make-interval a b) (cons a b))
-- Define selectors upper-bound and lower-bound to complete the implementation.
makeInterval :: (Integral a) => a -> a -> (a, a)
makeInterval lower upper = (lower, upper)

lowerBound :: (Integral a) => (a, a) -> a
lowerBound interval = fst interval

upperBound :: (Integral a) => (a, a) -> a
upperBound interval = snd interval


-- Exercise 2.8
-- Using reasoning analogous to Alyssa's, describe how the difference of two intervals may
-- be computed. Define a corresponding subtraction procedure, called sub-interval.
addInterval :: (Integral a) => (a, a) -> (a, a) -> (a, a)
addInterval x y = makeInterval ((lowerBound x) + (lowerBound y)) ((upperBound x) + (upperBound y))

subInterval :: (Integral a) => (a, a) -> (a, a) -> (a, a)
subInterval x y = addInterval x (makeInterval (-1 * (upperBound y)) (-1 * (lowerBound y)))


-- Exercise 2.17
-- Define a procedure last-pair that returns the list that contains only the last element of
-- a given (nonempty) list:
-- (last-pair (list 23 72 149 34)) --> (34)
-- NOTE: Syntactic sugar in Haskell allows for multiple ways to address basic list
-- functions. Whereas in Scheme, car and cdr and really the only list selectors, Haskell
-- allows for functions head, tail, init, last, and variations thereof. For the continuity
-- between SICP and Haskell, I will be sticking to the selectors head and tail, and their
-- syntactic variants (x:xs) (head:tail).
lastPair :: [a] -> [a]
lastPair (x:[]) = [x]
lastPair (x:xs) = lastPair(xs)


-- Exercise 2.18
-- Define a procedure reverse that takes a list as argument and returns a list of the same
-- elements in reverse order
-- NOTE: Reverse is predefined in the Standard Haskell Prelude, so I use the standard
-- ' notation to denote a function variation.
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse xs) ++ [x]


-- Exercise 2.20
-- The procedures +, *, and list take arbitrary numbers of arguments. One way to define such
-- procedures is to use define with dotted-tail notation. In a procedure definition, a
-- parameter list that has a dot before the last parameter name indicates that, when the
-- procedure is called, the initial parameters (if any) will have as values the initial
-- arguments, as usual, but the final parameter's value will be a list of any remaining
-- arguments.
-- Use this notation to write a procedure same-parity that takes one or more integers and
-- returns a list of all the arguments that have the same even-odd parity as the first
-- argument.
-- NOTE: Haskell does not support dotted tail notation, so this example will serve as an
-- implementation of filter.
sameParity :: (Integral a) => [a] -> [a]
sameParity [] = []
sameParity (x:xs) = if even x
                    then x : (filter' xs even)
                    else x : (filter' xs odd)

filter' :: [a] -> (a -> Bool) -> [a]
filter' [] predicate = []
filter' (x:xs) predicate = if predicate x
                           then x : (filter' xs predicate)
                           else filter' xs predicate


-- Exercise 2.23
-- The procedure for-each is similar to map. It takes as arguments a procedure and a list of
-- elements. However, rather than forming a list of the results, for-each just applies the
-- procedure to each of the elements in turn, from left to right. The values returned by
-- applying the procedure to the elements are not used at all -- for-each is used with
-- procedures that perform an action, such as printing. For example,
-- (for-each (lambda (x) (newline) (display x))
--           (list 57 321 88))
-- The value returned by the call to for-each (not illustrated above) can be something
-- arbitrary, such as true. Give an implementation of for-each.
forEach :: Monad m => [t] -> (t -> m a) -> m ()
forEach [] proc = return ()
forEach (x:xs) proc = do proc x
                         forEach xs proc


-- Exercise 2.27
-- Modify your reverse procedure of exercise 2.18 to produce a deep-reverse procedure that
-- takes a list as argument and returns as its value the list with its elements reversed and
-- with all sublists deep-reversed as well.
-- Note: Haskell lists must be pure (homogenous), so I will create a tree data structure, to
-- represent list depth.
data NestedList a = Leaf a
                  | Node [NestedList a]
     deriving (Eq, Show)

deepReverse :: NestedList a -> NestedList a
deepReverse (Leaf x)  = Leaf x
deepReverse (Node xs) = Node (reverse (map' deepReverse xs))

map' :: (t -> a) -> [t] -> [a]
map' proc [] = [];
map' proc (x:xs) = (proc x) : (map' proc xs)

-- Sample Nested List for 2.27
x = Node [Node [Leaf 1, Leaf 2], Node [Leaf 3, Leaf 4]]


-- Exercise 2.29
-- A binary mobile consists of two branches, a left branch and a right branch. Each branch
-- is a rod of a certain length, from which hangs either a weight or another binary mobile.
-- a. Write the corresponding selectors left-branch and right-branch, which return the
-- branches of a mobile, and branch-length and branch-structure, which return the components
-- of a branch.
-- b. Using your selectors, define a procedure total-weight that returns the total weight
-- of a mobile.
-- c. A mobile is said to be balanced if the torque applied by its top-left branch is equal
-- to that applied by its top-right branch (that is, if the length of the left rod
-- multiplied by the weight hanging from that rod is equal to the corresponding product for
-- the right side) and if each of the submobiles hanging off its branches is balanced.
-- Design a predicate that tests whether a binary mobile is balanced.
-----------------------------------------------------------------------------------
-- NOTE: This next part is very cool. I am going to use the data keyword here, to create a
-- sort of "abstraction" for a Mobile. I say "abstraction"
-- because the data constructor never creates objects, but instead, it creates a series of
-- functions binding a preset type to a type that we define. Feel free to check
-- (:t constructor).

data Mobile a = Mobile {mleft   :: Mobile a, mright  :: Mobile a}
              | Branch {mlen    :: Int,      mstruct :: Mobile a}
              | Weight {mweight :: a}
     deriving (Eq, Show)

-- a.
makeMobile :: Mobile a -> Mobile a -> Mobile a
makeMobile left right = Mobile {mleft=left, mright=right}

makeBranch :: Int -> Mobile a -> Mobile a
makeBranch len struct = Branch {mlen=len, mstruct=struct}

makeWeight :: a -> Mobile a
makeWeight weight = Weight {mweight=weight}

leftBranch :: Mobile t -> Mobile t
leftBranch (Mobile {mleft=left, mright=right}) = left

rightBranch :: Mobile t -> Mobile t
rightBranch (Mobile {mleft=left, mright=right}) = right

branchLength :: Mobile t -> Int
branchLength (Branch {mlen=len, mstruct=struct}) = len

branchStruct :: Mobile t -> Mobile t
branchStruct (Branch {mlen=len, mstruct=struct}) = struct

-- b.
totalWeight :: Num a => Mobile a -> a
totalWeight (Mobile {mleft = left, mright = right}) = (totalWeight left) + (totalWeight right)
totalWeight (Branch {mlen = _, mstruct = struct}) = totalWeight struct
totalWeight (Weight {mweight = weight}) = weight

-- c.
isBalanced :: Mobile Int -> Bool
isBalanced (Mobile {mleft = left, mright = right}) =
    let
        leftTorque = branchLength left * totalWeight left
        rightTorque = branchLength right * totalWeight right
    in
        leftTorque == rightTorque && isBalanced left && isBalanced right
isBalanced (Branch {mlen = _, mstruct = struct}) = isBalanced struct
isBalanced _ = True -- Anything that isn't a mobile is balanced.

-- Mobile example for testing:
m1 :: Mobile Int
m1 = makeMobile (makeBranch 10 (makeWeight 100))
                      (makeBranch 10 (makeMobile (makeBranch 40 (makeWeight 20))
                                                 (makeBranch 10 (makeWeight 80))))


-- Exercise 2.30
-- Define a procedure square-tree analogous to the square-list procedure of exercise 2.21.
-- NOTE: We will be using our NestedList for this (2.27)
squareTree :: (Num a) => NestedList a -> NestedList a
squareTree (Leaf x) = Leaf (x * x)
squareTree (Node xs) = Node (map squareTree xs)


-- Exercise 2.31
-- Abstract your answer to exercise 2.30 to produce a procedure tree-map with the property
-- that square-tree could be defined as
-- (define (square-tree tree) (tree-map square tree))
treeMap :: (a -> a) -> NestedList a -> NestedList a
treeMap proc (Leaf x) = Leaf (proc x)
treeMap proc (Node xs) = Node (map (treeMap proc) xs)


-- Exercise 2.32
-- Subsets Problem (a favorite of mine). Given a list x, return a list of lists of all
-- subsets of x.
-- This is a recurrence. Start with the base case (first subset): a null set. Then find all
-- subsets with one element (last element of the list). Then add the second element to every
-- existing subset. And so on, until the first element. Recursively, the solution is easy:
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (\sub -> x:sub) (subsets xs)
