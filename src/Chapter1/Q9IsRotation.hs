
module Q9IsRotation where

import Data.List
import Control.Monad

isRotation :: [Char] -> [Char] -> Bool
isRotation s1 s2 = length s1 == length s2 && s2 `isInfixOf` (s1 ++ s1)

-- point free solution
isRotation_2 :: [Char] -> [Char] -> Bool
isRotation_2 = ap (ap . ((&&) .) . (. length) . (==) . length) (flip isInfixOf . join (++))
