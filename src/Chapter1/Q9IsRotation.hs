
module Q9IsRotation where

import Data.List

isRotation :: [Char] -> [Char] -> Bool
isRotation s1 s2 = s2 `isInfixOf` (s1 ++ s1)

