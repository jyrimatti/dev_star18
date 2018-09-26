{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Ana where 

import Control.Category ((<<<)) -- function composition

import Prelude (Functor(fmap), (+))
import Numeric.Natural (Natural)
import Data.Functor.Foldable (Fix(..),cata,ana,ListF(..))

-- Stream of natural numbers
data StreamF r = StreamF Natural r deriving Functor
type NatStream = Fix StreamF

-- our "algorithm" to create numbers would be
naturalsCoalg n = StreamF n (n + 1) -- for each n, next is n+1

-- same as before, seed value zero
exampleNats = doStuff naturalsCoalg 0

-- unfortunately in this example our numbers are our NatStream,
-- but we can convert it to a Haskell list with a catamorphism
exampleNaturals = cata (\case StreamF x xs -> x:xs) exampleNats
-- > take 4 exampleNaturals
-- [0,1,2,3]

-- Dual of catamorphism:
-- 1) arrows are reversed
-- 2) wrap instead of unwrap
doStuff coalg = Fix <<< fmap (doStuff coalg) <<< coalg

-- "coalg" is called a "Coalgebra"
type Coalgebra f a = a -> f a

naturals :: [Natural]
naturals = ana (\n -> Cons n (n + 1)) 0
