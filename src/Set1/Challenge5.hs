module Set1.Challenge5 (encryptRepeatingXor) where

import Data qualified
import Data.Bits (xor)

encryptRepeatingXor :: String -> Data.Data -> Data.Data
encryptRepeatingXor key clear =
  xor full_key clear
  where
    full_key = Data.raw . take (Data.len clear) . cycle $ key
