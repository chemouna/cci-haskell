module Q8ZeroMatrix where

import Data.List (transpose, map, filter)

zeroMatrix :: [[Integer]] -> [[Integer]]
zeroMatrix xs = [[zero i j v | (j, v) <- zip [0..] row] | (i, row) <- zip [0..] xs]
  where
    zeroRows = zeroIndices xs
    zeroColumns = zeroIndices $ transpose xs
    zeroIndices y = map fst $ filter snd $ zip [0..] (map (any (== 0)) y)
    zero row col val | row `elem` zeroRows = 0
                     | col `elem` zeroColumns = 0
                     | otherwise = val
