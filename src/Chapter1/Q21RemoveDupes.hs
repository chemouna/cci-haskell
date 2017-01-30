
module Q21RemoveDupes where

removeDupes :: Eq a => [a] -> [a]
removeDupes [] = []
removeDupes (x:xs) = x:filter (/= x) (removeDupes xs)

