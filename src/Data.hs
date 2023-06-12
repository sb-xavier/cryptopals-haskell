module Data
  ( Data,
    arbitrary,
    raw,
    fromHex,
    fromBase64,
    toHex,
    toBase64,
    toBytes,
    toString,
    xor,
    len,
  )
where

import Data.Bifunctor
import qualified Data.Bits as Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.UTF8 as BU
import qualified Foreign as GHC
import qualified GHC.Word
import Numeric
import Test.QuickCheck (Arbitrary, arbitrary)

-- Generic functions to manipulate bytes and bits

data Data = Data B.ByteString deriving (Eq, Show)

pad :: String -> String
pad l =
  if length l `mod` 2 == 0
    then l
    else '0' : l

raw :: String -> Data
raw =
  Data . BU.fromString

fromHex :: String -> Data
fromHex h =
  Data $ B.pack $ map (\c -> toInt $ readHex c) $ pairUp $ pad h
  where
    toInt ((v, _) : _) = v
    toInt [] = 0
    pairUp (x : x' : t) = (x : x' : []) : (pairUp t)
    pairUp [_] = [] -- This only happens if your string is unpadded
    pairUp [] = []

toHex :: Data -> String
toHex (Data s) =
  B.foldl (\acc c -> acc ++ (pad $ showHex c "")) [] s

fromBase64 :: String -> Either String Data
fromBase64 h =
  second Data $ B64.decode $ BU.fromString h

toBase64 :: Data -> String
toBase64 (Data s) =
  BU.toString $ B64.encode s

toBytes :: Data -> [Int]
toBytes (Data d) = map fromIntegral $ B.unpack d

toString :: Data -> String
toString (Data d) = BU.toString d

xor :: Data -> Data -> Data
xor (Data d1) (Data d2) =
  Data $ B.pack $ map (\(a, b) -> Bits.xor a b) $ B.zip d1 d2

len :: Data -> Int
len (Data d) = B.length d

-- Allow QuickCheck testing of Data

instance Arbitrary Data where
  arbitrary = do
    s <- arbitrary
    return $ Data.raw s
