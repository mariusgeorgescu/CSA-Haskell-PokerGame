module Utilities where

import           Control.Monad   (foldM)
import           Data.Bifunctor  (Bifunctor (bimap))
import           Data.List       (nub)
import           Data.List.Extra (partition)
import           System.Random   (Random (randomR), RandomGen)

hasDuplicates :: (Eq a) => [a] -> Bool
hasDuplicates = (/=) <$> length <*> (length . nub)

splitByIndices :: (Foldable t, Eq a, Num a, Enum a) => t a -> [b] -> ([b], [b])
splitByIndices is xs =
  bimap (fmap snd) (fmap snd) $
  partition (\(i, _) -> i `elem` is) (zip [0 ..] xs)

updateList :: [a] -> Int -> a -> [a]
updateList xs i newVal = take i xs ++ [newVal] ++ drop (i + 1) xs

randomList :: RandomGen g => g -> Int -> [Int]
randomList gen n = go gen [0 .. n - 1]
  where
    go _ [] = []
    go g (x:xs) =
      let (r, g') = randomR (0, n - x - 1) g
       in r : go g' xs

repeatM :: (Monad m, Num a, Enum a) => a -> (t -> m t) -> t -> m t
repeatM n f a0 = foldM (\a _ -> f a) a0 [1 .. n]

