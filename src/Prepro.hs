{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
module Prepro where 

import Prelude (($), otherwise, Bool)
import Data.Functor.Foldable (prepro, ListF(Nil,Cons))

-- filtering of a List-functor with a predicate function
filter _ Nil = Nil
filter pred x@(Cons h _)
  | pred h    = x
  | otherwise = Nil

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile pred = prepro (filter pred) $ \case
  Nil         -> []
  (Cons x xs) -> x:xs -- transform recursive list to Haskell list