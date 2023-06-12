module Set1
  ( tests,
  )
where

import Data
import Set1.Challenge3 (decryptSingleCharXor)
import Set1.Challenge4 (findSingleCharXor)
import Test.Hspec

challenge1 :: IO ()
challenge1 =
  b `shouldBe` Right a
  where
    a = fromHex "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    b = fromBase64 "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

challenge2 :: IO ()
challenge2 =
  (a `xor` b) `shouldBe` c
  where
    a = fromHex "1c0111001f010100061a024b53535009181c"
    b = fromHex "686974207468652062756c6c277320657965"
    c = fromHex "746865206b696420646f6e277420706c6179"

challenge3 :: IO ()
challenge3 =
  decryptSingleCharXor cipher `shouldBe` (88, raw "Cooking MC's like a pound of bacon")
  where
    cipher = fromHex "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

challenge4 :: IO ()
challenge4 =
  do
    inputs <- readFile "data/4.txt"
    findSingleCharXor (map fromHex $ lines inputs)
      `shouldBe` Just (53, raw "Now that the party is jumping\n")

tests :: SpecWith ()
tests = do
  describe "Set 1" $ do
    it "Challenge 1" $ do
      challenge1

    it "Challenge 2" $ do
      challenge2

    it "Challenge 3" $ do
      challenge3

    it "Challenge 4" $ do
      challenge4
