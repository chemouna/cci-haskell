
module Q7MatrixRotate90 where

import Data.List (transpose)

rotate90 :: [[a]] -> [[a]]
rotate90 = fmap reverse . transpose

rotate90_2 :: [[a]] -> [[a]]
rotate90_2 = transpose . reverse

