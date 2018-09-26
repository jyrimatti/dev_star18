{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Histo where 

import Control.Category ((>>>)) -- function composition
import Control.Category.Cartesian ((&&&)) -- fanout
import Control.Comonad.Cofree (Cofree(..))

import Prelude (Functor(fmap), (+), ($), id, Maybe(..), uncurry)
import Numeric.Natural (Natural)
import Data.Functor.Foldable (histo,unfix)

import Para (NatF(ZeroF,SuccF)) -- use the Nat functor defined earlier

-- using Cofree Comonad here to bring comonadic structure...
fibonacciAlg ZeroF                         = 0
fibonacciAlg (SuccF (_ :< ZeroF))          = 1
fibonacciAlg (SuccF (a :< SuccF (b :< _))) = a + b

exampleFibonacci = doStuff fibonacciAlg
-- > exampleFibonacci (succ (succ (succ zero))) -- 3
-- 2

doStuff alg = worker >>> (\(x :< _) -> x)
  where worker = unfix >>> fmap worker >>> (alg &&& id) >>> uncurry (:<)

-- here "alg" is called a "CVAlgebra"
type CVAlgebra f a = f (Cofree f a) -> a

-- Complete implementation using recursion-schemes library:
fibonacci :: Natural -> Natural
fibonacci = histo $ \case
  Nothing                   -> 0
  Just (_ :< Nothing)       -> 1
  Just (a :< Just (b :< _)) -> a + b
