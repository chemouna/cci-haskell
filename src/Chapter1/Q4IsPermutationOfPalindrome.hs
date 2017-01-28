module Q4IsPermutationOfPalindrome where

import Data.List
import Data.Char (toLower)

-- Sol 1
isPalindrome :: String -> Bool
isPalindrome x = x == reverse x

isPermutationOfPalindrome :: String -> Bool
isPermutationOfPalindrome x = (length $ filter isPalindrome (permutations cleanString :: [String])) > 0
  where cleanString = (filter (/= ' ') $ map toLower x) :: String
