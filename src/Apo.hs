{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
module Apo where 

import Control.Category ((<<<)) -- function composition

import Control.Category.Cartesian ((|||)) -- fanin

import Prelude (Functor(fmap), (+), ($), id, Int, (==),Either(..), flip)
import Data.Functor.Foldable (Fix(..),cata,apo,Fix(..),ListF(..))

import Cata(MyListF(..),nil)

rangeCoalg to = \case
  n | n == to -> ConsF n (Left nil)
  n           -> ConsF n (Right (n + 1))

exampleRange_ from to = doStuff (rangeCoalg to) from
-- have to convert our List type to the Haskell list again...
exampleRange from to = cata (\case
  NilF       -> []
  ConsF x xs -> x:xs
 ) (exampleRange_ from to)
-- > exampleRange 0 4
-- [0,1,2,3,4]

doStuff coalg = Fix <<< fmap (id ||| doStuff coalg) <<< coalg

-- here "coalg" is called an "RCoalgebra":
type RCoalgebra f a = a -> f (Either (Fix f) a)

-- Complete implementation using recursion-schemes library:
range :: Int -> Int -> [Int]
range from to = flip apo from $ \case
  n | n == to -> Cons n (Left [])
  n           -> Cons n (Right (n + 1))