{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List
import           Data.List.Split

main = interact $ show . total . parse

parse = fmap (fmap tail . break (== ' ')) . lines

points a "Y" = bonus a -- draw
points a "Z" = bonus $ win a -- win
points a "X" = bonus $ win $ win a -- lose

bonus "A" = 1
bonus "B" = 2
bonus "C" = 3

win "A" = "B"
win "B" = "C"
win "C" = "A"

outcome = \case
  "X" -> 0
  "Y" -> 3
  "Z" -> 6

total = sum . fmap (\(a, b) -> points a b + outcome b)
