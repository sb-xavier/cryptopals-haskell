module Util.List (rotate, every, chunksOf) where

rotate :: [a] -> Int -> [a]
rotate xs n
  | l == 0 = []
  | n < 0 = rotate xs (l + (n `mod` l))
  | n == 0 = xs
  | otherwise = drop (n `mod` l) xs <> take (n `mod` l) xs
  where
    l = length xs

every :: Int -> Int -> [a] -> [a]
every _ _ [] = []
every 0 n xs = head xs : every 0 n (drop n xs)
every offset n xs = every 0 n (drop offset xs)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
