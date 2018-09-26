{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Cata where 

import Control.Category ((>>>)) -- function composition

import Prelude (Functor(fmap), (+), ($), Int)
import Data.Functor.Foldable (Fix(..), unfix, cata, ListF(Nil,Cons))

-- Familiar cons-list. We don't have to write this, but for the sake of an example...
data MyListF a r = NilF | ConsF a r deriving Functor

type List a = Fix (MyListF a)
nil = Fix NilF -- make an empty list
cons x xs = Fix (ConsF x xs) -- cons a value in front of a list



-- our "algorithm" to sum list elements would be just:
sumAlg = \case
    NilF       -> 0      -- sum of an empty list
    ConsF x xs -> x + xs -- for other element we add the current value to the rest of the values

-- we should be able to execute our algorithm by giving it with the list to some function called "doStuff"
exampleSum = doStuff sumAlg
-- > exampleSum (cons 4 $ cons 12 $ cons 7 nil) -- [4, 12, 7]
-- 23

-- This general library function still needs explicit recursion
doStuff alg = unfix >>>          -- unwrap from the Fix point
              fmap (doStuff alg) -- apply algorithm recursively
              >>> alg            -- apply to the final value

-- "alg" was not an abreviation of "algorithm", but of "Algebra"
type Algebra f a = f a -> a

-- Complete implementation for 'sum' would thus be the
-- following (using recursion-schemes library), which neatly
-- contains only the algorithmic part without any recursion
sum :: [Int] -> Int
sum = cata $ \case
    Nil       -> 0
    Cons x xs -> x + xs
