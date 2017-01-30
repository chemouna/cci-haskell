module Q21RemoveDupes where

import Data.List

removeDupes :: Eq a => [a] -> [a]
removeDupes [] = []
removeDupes (x:xs) = x:filter (/= x) (removeDupes xs)

removeDupes2 :: Eq a => [a] -> [a]
removeDupes2 = foldl (\seen x -> if x `elem` seen
                                 then seen
                                 else seen ++ [x]) []
