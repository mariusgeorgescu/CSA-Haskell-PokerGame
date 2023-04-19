module Utilities where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (nub)
import Data.List.Extra (partition)

hasDuplicates :: (Eq a) => [a] -> Bool
hasDuplicates = (/=) <$> length <*> (length . nub)

splitByIndices :: (Foldable t, Eq a, Num a, Enum a) => t a -> [b] -> ([b], [b])
splitByIndices is xs =
  bimap (fmap snd) (fmap snd) $
    partition (\(i, _) -> i `elem` is) (zip [0 ..] xs)

updateList :: [a] -> Int -> a -> [a]
updateList xs i newVal = take i xs ++ [newVal] ++ drop (i + 1) xs