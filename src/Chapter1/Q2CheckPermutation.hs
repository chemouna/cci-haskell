module Q2CheckPermutation where

import Data.List
-- import Test.Hspec
-- import Test.QuickCheck
-- import Test.QuickCheck.All

isPermutation :: String -> String -> Bool
isPermutation x y = sort x == sort y


-- Test
isPermutationSpec :: Spec
isPermutationSpec = do
  describe "isPermutation" $ do
    it "checks a string is a permutation of another." $ do
      isPermutation "ab" "ba"
        `shouldBe` True

-- prop_encodeModifiedSameSize xs = length (encode xs) == length (encodeModified xs)

-- return []
-- main = $quickCheckAll
