module DataHelpers (tests) where

import Test.Hspec
import Test.QuickCheck
import Util.List (rotate)

testRotate :: Int -> IO ()
testRotate i =
  rotate (rotate l i) (negate i) `shouldBe` l
  where
    l = [1 :: Int .. 10]

tests :: SpecWith ()
tests = do
  describe "Data helpers" $ do
    it "List rotate" $
      property testRotate
