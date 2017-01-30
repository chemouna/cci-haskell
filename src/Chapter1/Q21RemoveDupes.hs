module Q21RemoveDupes where

import Data.List
import qualified Data.Set as Set

removeDupes :: Eq a => [a] -> [a]
removeDupes [] = []
removeDupes (x:xs) = x:filter (/= x) (removeDupes xs)

-- O(n^2) but with Eq constraint its not possible to have an O(n) solution
removeDupes2 :: Eq a => [a] -> [a]
removeDupes2 = foldl (\seen x -> if x `elem` seen
                                 then seen
                                 else seen ++ [x]) []

-- an O(n log n) solution but that requires Ord constraint and doesnt preserve the order
removeDupes3 :: Ord a => [a] -> [a]
removeDupes3 = map head . group . sort

-- another O(n log n) solution that requires Ord constraint but preserves the order
removeDupes4 :: Ord a => [a] -> [a]
removeDupes4 = rmdupes Set.empty where
  rmdupes _ [] = []
  rmdupes a (b : c) = if Set.member b a
                         then rmdupes a c
                         else b : rmdupes (Set.insert b a) c
