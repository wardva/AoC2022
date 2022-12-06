{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Char              (isDigit)
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Vector            ((!))
import qualified Data.Vector            as V
import           Text.Regex.Applicative

type Stacks = V.Vector [Char]
type Moves = [(Int, Int, Int)]

main = interact $ solve . parse

parse :: String -> (Stacks, Moves)
parse input =
  let [stacks, moves] = splitOn "\n\n" input
  in (parseStacks stacks, parseMove <$> lines moves)

parseStacks :: String -> Stacks
parseStacks =
  V.fromList
    . filter (not . null)
    . fmap (filter includeChar)
    . transpose
    . lines

 where
   includeChar '\n' = True
   includeChar c    = c >= 'A' && c <= 'Z'

parseMove :: String -> (Int, Int, Int)
parseMove = fromJust . match moveRegex

  where
    intRegex = read <$> many (psym isDigit)
    moveRegex = (,,)
      <$> (string "move " *> intRegex)
      <*> (string " from " *> (pred <$> intRegex))
      <*> (string " to " *> (pred <$> intRegex))

solve :: (Stacks, Moves) -> String
solve = fmap head . V.toList . uncurry (foldl applyMove)

  where
    applyMove stacks (n, from, to) =
      let (moveElems, fromStack') = splitAt n (stacks ! from)
      in V.update stacks $ V.fromList [(from, fromStack'), (to, moveElems <> stacks ! to)]
