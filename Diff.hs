module Diff (
    diff,
    Diff(Del,Mod,Add)
) where

import Data.List(sortBy)
import Data.Set(fromList,toList,(\\))
import Data.Function (on)
import Control.Parallel(par,pseq)

data Diff a = Del a | Mod a a | Add a deriving (Show)

instance Functor Diff where
  fmap f (Del a)   = Del (f a)
  fmap f (Mod a b) = Mod (f a) (f b)
  fmap f (Add a)   = Add (f a)

getContent :: Diff a -> a
getContent (Del a)   = a
getContent (Add a)   = a
getContent (Mod _ a) = a

foldOnContent :: Eq b => (a -> b) -> [Diff a] -> [Diff a]
foldOnContent id' (Add a:Del b:xs)
    | (id' a) == (id' b) = Mod b a : foldOnContent id' xs
foldOnContent id' (Del a:Add b:xs)
    | (id' a) == (id' b) = Mod a b : foldOnContent id' xs
foldOnContent id' (x:xs) = x : foldOnContent id' xs
foldOnContent _ []       = []

diff :: (Ord a, Ord i) => (a -> i) -> [a] -> [a] -> [Diff a]
diff id' xs ys = xs `seq`
                 ys `seq`
                 foldOnContent id' $
                 sortBy compareOnContent diffList
    where
        del = map Del $ toList $ (fromList xs) \\ (fromList ys)
        add = map Add $ toList $ (fromList ys) \\ (fromList xs)
        diffList = del `par` add `pseq` (del ++ add)
        compareOnContent = compare `on` (id' . getContent)
