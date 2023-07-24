module DataHelpers (tests) where

import Data (lrotate)
import Test.Hspec
import Test.QuickCheck

testLRotate :: Int -> IO ()
testLRotate i =
  lrotate (lrotate l i) (negate i) `shouldBe` l
  where
    l = [1 :: Int .. 10]

tests :: SpecWith ()
tests = do
  describe "Data helpers" $ do
    it "List rotate" $
      property testLRotate
