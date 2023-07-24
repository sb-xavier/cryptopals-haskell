module Set1.Challenge3
  ( decryptSingleCharXor,
    scoring,
  )
where

import Data
import Data.Bits (xor)
import Data.Char
import Data.List qualified as L
import Data.Maybe (fromMaybe)

scoring :: Data -> Int
scoring input =
  sum $ map score $ toBytes input
  where
    reference = map ord $ reverse "ETAOIN SHRDLU"
    toUpperInt i
      | i > 96 && i < 123 = i - 32
      | otherwise = i
    score c =
      fromMaybe (-1) $ L.elemIndex (toUpperInt c) reference

decryptSingleCharXor :: Data -> (Int, Data)
decryptSingleCharXor cipher =
  ( bestChar,
    xor cipher $
      repeatChar (chr bestChar)
  )
  where
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
