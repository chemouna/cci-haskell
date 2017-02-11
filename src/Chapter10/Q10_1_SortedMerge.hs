{-# LANGUAGE TemplateHaskell #-}

module Q10_1_SortedMerge where

import Data.List
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Arbitrary

sortedMerge :: Ord a => [a] -> [a] -> [a]
sortedMerge [] xs = xs
sortedMerge xs [] = xs
sortedMerge (x:xs) (y:ys)
  | x <= y = x:sortedMerge xs (y:ys)
  | otherwise = y:sortedMerge (x:xs) ys

prop_sortedMerge_min_is_min_two_list (Ordered xs) (Ordered ys) =
  not (null xs) && not (null ys) ==> minimum (sortedMerge xs ys) == minimum [head xs, head ys]

prop_sortedMerge_max_is_max_two_list (Ordered xs) (Ordered ys) =
  not (null xs) && not (null ys) ==> maximum (sortedMerge xs ys) == maximum [last xs, last ys]

prop_sortedMerge_equals_size_of_both xs ys = length (sortedMerge xs ys) == length xs + length ys

return []
main = $quickCheckAll

