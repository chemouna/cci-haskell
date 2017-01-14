
module Q4IsPermutationOfPalindrome where

import Data.List

isPalindrome :: String -> Bool
isPalindrome x = x == reverse x

isPermutationOfPalindrome :: String -> Bool
isPermutationOfPalindrome x = (length $ filter isPalindrome (permutations (filter (/= ' ') x) :: [String])) > 0


