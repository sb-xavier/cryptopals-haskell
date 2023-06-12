module Set1.Challenge3 (decryptSingleCharXor) where

import Data
import Data.Char
import qualified Data.List as L

scoring :: Data -> Int
scoring input =
  sum $ map score $ toBytes input
  where
    reference = map ord $ reverse "ETAOIN SHRDLU"
    toUpperInt i
      | i > 96 && i < 123 = i - 32
      | otherwise = i
    score c =
      case L.elemIndex (toUpperInt c) reference of
        Just i -> i + 1
        _ -> 0

decryptSingleCharXor :: String -> (Int, String)
decryptSingleCharXor hCipher =
  ( bestChar,
    toString $
      xor cipher $
        repeatChar (chr bestChar)
  )
  where
    cipher = fromHex hCipher
    size = len cipher
    repeatChar c =
      raw $ replicate size c
    allScores =
      map (scoring . xor cipher . repeatChar) ['\0' .. '\255']
    bestScore =
      maximum allScores
    bestChar =
      case L.elemIndex bestScore allScores of
        Just i -> i
        _ -> 0
