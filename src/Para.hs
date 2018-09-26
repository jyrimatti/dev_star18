{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Para where 

import Control.Category ((>>>)) -- function composition

import Control.Category.Cartesian ((&&&)) -- fanout

import Prelude (Functor(fmap), (+), (*), ($), (.), id,Maybe(..))
import Numeric.Natural (Natural)
import Data.Functor.Foldable (Fix(..),cata,para,unfix)

-- Natural numbers as Peano
data NatF r = ZeroF | SuccF r deriving Functor
type Nat = Fix NatF

zero = Fix ZeroF
succ = Fix . SuccF

factorialAlg = \case
    ZeroF          -> 1 -- factorial of zero is one
    SuccF (cur, r) -> (toNat cur + 1) * r
  -- unfortunately our example numbers are Peano, but we
  -- can use catamorphism to transform to regular numbers
  where toNat = cata $ \case
          ZeroF   -> 0
          SuccF n -> n + 1

exampleFactorial = doStuff factorialAlg
-- > exampleFactorial (succ (succ (succ zero))) -- 3
-- 6

doStuff alg = unfix >>> fmap (id &&& doStuff alg) >>> alg

-- here "alg" is called an "RAlgebra"
type RAlgebra f a = f (Fix f, a) -> a

factorial :: Natural -> Natural
factorial = para $ \case
  Nothing     -> 1
  Just (n, r) -> (n+1) * r
