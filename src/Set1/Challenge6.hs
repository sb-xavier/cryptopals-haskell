module Set1.Challenge6 (RepeatingXorParameters (..), decryptRepeatingXor, defaultParams) where

import Data
import Data.Char (chr)
import Data.Function (on, (&))
import GHC.Exts (sortWith)
import Set1.Challenge3 (decryptSingleCharXor, scoring)
import Util.List qualified as UL

data RepeatingXorParameters = RepeatingXorParameters
  { minsize :: Int,
    maxsize :: Int,
    initialDepth :: Int,
    candidates :: Int
  }
  deriving (Eq, Show)

defaultParams :: RepeatingXorParameters
defaultParams = RepeatingXorParameters {minsize = 1, maxsize = 8, initialDepth = 4, candidates = 2}

decryptRepeatingXor :: RepeatingXorParameters -> Data -> (Data, Data)
decryptRepeatingXor params cipher =
  [minsize params .. maxsize params]
    & sortWith (averageDistanceOverFirstBlocks cipher $ initialDepth params)
    & take (candidates params)
    & map (decryptWithKeySize cipher)
    & sortWith (\(_, c) -> scoring c)
    & last
  where
    averageDistanceOverFirstBlocks :: Data -> Int -> Int -> Float
    averageDistanceOverFirstBlocks c depth blocksize =
      fdiv
        ( sum
            ( map
                (\i -> hamming (slice 0 blocksize c) (slice (blocksize * i) (blocksize * (i + 1)) c))
                [0 .. depth]
            )
        )
        depth
    decryptWithKeySize :: Data -> Int -> (Data, Data)
    decryptWithKeySize c keysize =
      [0 .. (keysize - 1)]
        & map (decryptSingleCharXor . (\offset -> Data.every offset keysize c))
        & foldr
          (\(k, d) (acc_key, acc_clear) -> (chr k : acc_key, toBitString d <> acc_clear))
          ([], [])
        & ( \(key, bits) ->
              ( raw key,
                fromBitString $ concatMap (\offset -> UL.every offset keysize bits) [0 .. keysize - 1]
              )
          )
    fdiv = (/) `on` fromIntegral
