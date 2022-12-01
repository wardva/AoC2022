{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List
import           Data.List.Split

main = interact $ show . solve . parse
parse = fmap (fmap read . lines) . splitOn "\n\n"
solve = sum . take 3 . reverse . sort . fmap sum
