module Util.String (pad, toWord, fill) where

import Data.List (foldl')
import Data.Word (Word8)

pad :: Int -> String -> String
pad n l =
  -- 0-pad a string to a length that is a multiple of n
  if length l `mod` n == 0
    then l
    else pad n $ '0' : l

fill :: Int -> String -> String
fill n l =
  -- 0-pad a string to a length that is a multiple of n
  if length l `mod` n == 0
    then l
    else l <> replicate (n - length l `mod` n) '0'

bitToWord :: Char -> Word8
bitToWord '0' = 0
bitToWord '1' = 1
bitToWord _ = 0

toWord :: String -> Word8
toWord = foldl' (\acc x -> acc * 2 + bitToWord x) 0 . reverse
