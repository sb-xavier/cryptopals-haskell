module Set1.Challenge5 (encryptRepeatingXor) where

import qualified Data

encryptRepeatingXor :: String -> Data.Data -> Data.Data
encryptRepeatingXor key clear =
  Data.xor full_key clear
  where
    full_key = Data.raw . take (Data.len clear) . cycle $ key
