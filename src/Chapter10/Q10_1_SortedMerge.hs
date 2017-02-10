{-# LANGUAGE TemplateHaskell #-}

module Q10_1_SortedMerge where

import Test.QuickCheck
import Test.QuickCheck.All

sortedMerge :: Ord a => [a] -> [a] -> [a]
sortedMerge [] xs = xs
sortedMerge xs [] = xs
sortedMerge (x:xs) (y:ys)
  | x <= y = x:sortedMerge xs (y:ys)
  | otherwise = y:sortedMerge (x:xs) ys

-- TODO: how in quicksort to write the invariant that the two lists must be ordered each

prop_sortedMerge_min_is_min_two_list xs ys =
  not (null xs) && not (null ys) ==> minimum (sortedMerge xs ys) == minimum [head xs, head ys]

return []
main = $quickCheckAll

