
module Q22ReturnKthToLast where

kth :: Int -> [a] -> a
kth k = (!! k) . reverse

-- pointfree
kth_2 :: Int -> [a] -> a
kth_2 = (. reverse) . flip (!!)
