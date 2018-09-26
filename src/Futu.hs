{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
module Futu where 

import Control.Category ((<<<)) -- function composition
import Control.Monad.Free (Free(Pure,Free))

import Prelude (Functor(fmap), ($), (.), words, String, unwords, head, tail)
import Data.Char (toUpper)
import Data.Functor.Foldable (Fix(..),futu,cata,ListF(..))

import Cata(MyListF(..)) -- use the List functor defined earlier

capitalizeWordsCoalg ""     = NilF
capitalizeWordsCoalg str = let ws = words str
                               (first,rest) = (head ws, tail ws)
                               capitalized = toUpper (head first) : tail first
                           in ConsF capitalized (Pure $ unwords rest)

exampleCapitalizeWords_ = doStuff capitalizeWordsCoalg
exampleCapitalizeWords str = unwords $ cata (\case
  NilF       -> []
  ConsF x xs -> x:xs
 ) (exampleCapitalizeWords_ str)
-- > exampleCapitalizeWords "foo bar"
-- "Foo Bar"

doStuff coalg = Fix <<< fmap worker <<< coalg
  where worker (Pure a) = doStuff coalg a
        worker (Free a) = Fix (fmap worker a)

-- here "coalg" is called a "CVCoalgebra":
type CVCoalgebra f a = a -> f (Free f a)

-- Complete implementation using recursion-schemes library:
capitalizeWords :: String -> String
capitalizeWords = unwords . futu (\case
  ""  -> Nil
  str -> let ws = words str
             (first,rest) = (head ws, tail ws)
             capitalized = toUpper (head first) : tail first
         in Cons capitalized (Pure $ unwords rest))