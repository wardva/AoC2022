module Main where

import           Data.List
import           Data.List.Split

main = interact $ show . solve 0

searchLength = 14

solve n cs@(_:nxt)
  | length (nub prefix) == searchLength =
      n + searchLength
  | otherwise = solve (n+1) nxt

  where
    (prefix, _) = splitAt searchLength cs
