module DataTests (tests) where

import Data
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

testHex :: Data -> IO ()
testHex s =
  (Data.fromHex . Data.toHex $ s) `shouldBe` s

testBase64 :: Data -> IO ()
testBase64 s =
  (Data.fromBase64 . Data.toBase64 $ s) `shouldBe` (Right s)

tests :: SpecWith ()
tests = do
  describe "Data type" $ do
    modifyMaxSuccess (const 10000) $
      it "Hex encoding" $
        property $
          testHex

    modifyMaxSuccess (const 10000) $
      it "Base64 encoding" $
        property $
          testBase64
