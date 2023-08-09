module Utilities
  ( hasDuplicates,
    splitByIndices,
    repeatM,
    randomList,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (Bifunctor (bimap))
import Data.List (nub)
import Data.List.Extra (partition)
import System.Random (Random (randomR), RandomGen)

-- | Checks if a list contains duplicates
hasDuplicates :: (Eq a) => [a] -> Bool
hasDuplicates = (/=) <$> length <*> (length . nub)

-- |  Takes a list of indices and a list, and it returns a tuple of two lists.
-- The first list contains elements at the given indices, and the second list contains the rest of the elements.
splitByIndices :: (Foldable t, Eq a, Num a, Enum a) => t a -> [b] -> ([b], [b])
splitByIndices is xs =
  bimap (fmap snd) (fmap snd) $
    partition (\(i, _) -> i `elem` is) (zip [0 ..] xs)

-- | Generates a random list of length n
randomList :: (RandomGen g) => g -> Int -> [Int]
randomList gen n = go gen [0 .. n - 1]
  where
    go _ [] = []
    go g (x : xs) =
      let (r, g') = randomR (0, n - x - 1) g
       in r : go g' xs

-- | Aapplies the function f to the value a0, n times in the context of the m monad.
repeatM :: (Monad m, Num a, Enum a) => a -> (t -> m t) -> t -> m t
repeatM n f a0 = foldM (\a _ -> f a) a0 [1 .. n]
