{-# LANGUAGE NoImplicitPrelude #-}
module Postpro where 

import Prelude (Num, Ord, (+), (<=))
import Data.Functor.Foldable (postpro, ListF(Cons))

import Prepro (filter) -- use the same filter function

-- use whatever generator function
generate f n = Cons n (f n)

-- now both filtering and stream generation are nicely separated from recursion
range :: (Num a, Ord a) => a -> a -> [a]
range from to = postpro (filter (<= to)) (generate (+1)) from
