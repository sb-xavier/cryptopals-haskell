module Set1.Challenge4 (findSingleCharXor) where

import Data
import Data.List (find)
import Set1.Challenge3

findSingleCharXor :: [Data] -> Maybe (Int, Data)
findSingleCharXor inputs =
  find (\(_, clear) -> scoring clear == best_score) cleartexts
  where
    cleartexts = map decryptSingleCharXor inputs
    all_scores = map (scoring . snd) cleartexts
    best_score = maximum all_scores
