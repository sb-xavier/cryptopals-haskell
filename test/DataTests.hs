module DataTests (tests) where

import Data
import Data.Bits (popCount, rotate, rotateL, rotateR, shiftL, shiftR, xor)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

testHex :: Data -> IO ()
testHex s =
  (Data.fromHex . Data.toHex $ s) `shouldBe` s

testBase64 :: Data -> IO ()
testBase64 s =
  (Data.fromBase64 . Data.toBase64 $ s) `shouldBe` Right s

testRotatePosNeg :: Int -> Data -> IO ()
testRotatePosNeg i d =
  rotate (rotate d i) (negate i) `shouldBe` d

testShiftPosNeg :: Int -> Data -> IO ()
testShiftPosNeg i d =
  shiftR (shiftL d (abs i)) (abs i) `shouldBe` d

testRotateL :: Int -> Data -> IO ()
testRotateL i d =
  rotateR (rotateL d (abs i)) (abs i) `shouldBe` d

testRotateR :: Int -> Data -> IO ()
testRotateR i d =
  rotateL (rotateR d (abs i)) (abs i) `shouldBe` d

testSubwordRotation :: Data -> IO ()
testSubwordRotation d = do
  rotate (rotate d 1) (-1) `shouldBe` d
  rotate (rotate d 2) (-2) `shouldBe` d
  rotate (rotate d 3) (-3) `shouldBe` d
  rotate (rotate d 4) (-4) `shouldBe` d
  rotate (rotate d 5) (-5) `shouldBe` d
  rotate (rotate d 6) (-6) `shouldBe` d
  rotate (rotate d 7) (-7) `shouldBe` d

testRotationExamples :: IO ()
testRotationExamples = do
  showBin (rotate (fromHex "ff0") 0) `shouldBe` "1111000000001111"
  showBin (rotate (fromHex "ff0") 6) `shouldBe` "0000001111111100"
  showBin (rotateL (fromHex "ff0") 6) `shouldBe` "0000001111111100"
  showBin (rotateR (fromHex "ff0") 6) `shouldBe` "0011111111000000"
  showBin (rotateR (fromHex "ff0") 9) `shouldBe` "0000011111111000"

testSelfXor :: Data -> IO ()
testSelfXor d =
  popCount (xor d d) `shouldBe` 0

tests :: SpecWith ()
tests = do
  describe "Data type" $ do
    modifyMaxSuccess (const 1000) $
      it "Hex encoding" $
        property testHex

    modifyMaxSuccess (const 1000) $
      it "Base64 encoding" $
        property testBase64

    describe "Bitwise operations" $ do
      it "Sub-word rotations" $
        property testSubwordRotation

      it
        "Rotation examples"
        testRotationExamples

      it "Xor with self" $
        property testSelfXor

  describe "Rotation" $ do
    it "Positive then negative rotation" $
      property testRotatePosNeg

    it "Rotate left, then right" $
      property testRotateL

    it "Rotate right, then left" $
      property testRotateR

  describe "Shift" $ do
    it "Left then right shift" $
      property testShiftPosNeg
