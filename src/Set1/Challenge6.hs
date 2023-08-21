{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Fuse foldr/map" #-}
module Set1.Challenge6 (RepeatingXorParameters (..), decryptRepeatingXor, defaultParams) where

import Data
import Data.Char (chr)
import Data.Function (on, (&))
import Debug.Trace (trace)
import GHC.Exts (sortWith)
import Set1.Challenge3 (decryptSingleCharXor, scoring)
import Set1.Challenge5 (encryptRepeatingXor)

data RepeatingXorParameters = RepeatingXorParameters
  { minsize :: Int,
    maxsize :: Int,
    initialDepth :: Int,
    candidates :: Int
  }
  deriving (Eq, Show)

defaultParams :: RepeatingXorParameters
defaultParams = RepeatingXorParameters {minsize = 2, maxsize = 30, initialDepth = 2, candidates = 2}

decryptRepeatingXor :: RepeatingXorParameters -> Data -> (Data, Data)
decryptRepeatingXor params cipher =
  [minsize params .. maxsize params]
    & sortWith (averageDistanceOverFirstBlocks cipher $ initialDepth params)
    & take (candidates params)
    & map (decryptWithKeySize cipher)
    & sortWith (\(_, c) -> trace ("score " <> show (scoring c) <> " " <> show c) $ scoring c)
    & last
  where
    averageDistanceOverFirstBlocks :: Data -> Int -> Int -> Float
    averageDistanceOverFirstBlocks c depth blocksize =
      fdiv
        ( sum
            ( map
                (\i -> hamming (slice 0 blocksize c) (slice (blocksize * i) (blocksize * (i + 1)) c))
                [1 .. depth]
            )
        )
        (blocksize * depth)
    decryptWithKeySize :: Data -> Int -> (Data, Data)
    decryptWithKeySize c keysize =
      [0 .. (keysize - 1)]
        & map (decryptSingleCharXor . (\offset -> every offset keysize c))
        & foldr
          (\(k, d) (acc_key, acc_clear) -> (chr k : acc_key, d <> acc_clear))
          ([], mempty)
        & ( \(key, _) ->
              (raw key, encryptRepeatingXor key c)
          )
    fdiv = (/) `on` fromIntegral
