module Set1.Challenge6 (decryptRepeatingXor) where

import Data
import Data.Function (on, (&))
import GHC.Exts (sortWith)
import Set1.Challenge3 (scoring)

data RepeatingXorParameters = RepeatingXorParameters
  { maxsize :: Int,
    minsize :: Int,
    initialDepth :: Int,
    candidates :: Int
  }
  deriving (Eq, Show)

every _ [] = []
every n xs = head xs : every n (drop n xs)

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
    decryptWithKeySize c keysize =
      [0 .. (keysize - 1)]
        & map (\offset -> [])
    fdiv = (/) `on` fromIntegral
