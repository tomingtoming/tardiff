module Diff (
    diff, getContent,
    Diff(Del,Mod,Add)
) where

import qualified Data.Map as Map

data Diff a = Del a | Mod a a | Add a --deriving (Show)

instance Show a => Show (Diff a) where
  show (Del a)   = concat ["[Del]", show a]
  show (Mod a b) = concat ["[Mod]", show a, "\n--->>", show b]
  show (Add a)   = concat ["[Add]", show a]

instance Functor Diff where
  fmap f (Del a)   = Del (f a)
  fmap f (Mod a b) = Mod (f a) (f b)
  fmap f (Add a)   = Add (f a)

getContent :: Diff a -> a
getContent (Del a)   = a
getContent (Add a)   = a
getContent (Mod _ a) = a

diff :: (Eq a, Ord i) => (a -> i) -> [a] -> [a] -> [Diff a]
diff identify xs ys =
  let
    xm = Map.fromList $ zip (map identify xs) xs
    ym = Map.fromList $ zip (map identify ys) ys
    rm = Map.map Del $ Map.difference xm ym
    ch = Map.filter (not . isSame) $ Map.intersectionWith (\x y -> Mod x y) xm ym
    mk = Map.map Add $ Map.difference ym xm
  in
    Map.elems $ Map.unions [rm, ch, mk]

isSame :: Eq a => Diff a -> Bool
isSame (Del _)   = False
isSame (Mod x y) = x == y
isSame (Add _)   = False
