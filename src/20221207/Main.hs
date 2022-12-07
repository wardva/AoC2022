{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Data.Functor
import Data.List.Split
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map

main = interact $ show . solve . tail . lines
solve = deleteDirSize . buildFilesystem

data Node
  = File { nodeName :: String, fileSize :: Int }
  | Dir { nodeName :: String, dirNodes :: Map.Map String Node }

type NodeZipper = (Node, [(String, Map.Map String Node)])

rootNode = Dir "/" mempty
zipper root = (root, [])

top :: NodeZipper -> NodeZipper
top z@(Dir "/" _, []) = z
top z = top $ nodeUp z

nodeUp :: NodeZipper -> NodeZipper
nodeUp (item, (name, nodes):locs) =
  (Dir name (Map.insert (nodeName item) item nodes), locs)

nodeDown :: String -> NodeZipper -> NodeZipper
nodeDown name (Dir dirName children, locs) =
  (children ! name, (dirName, Map.delete name children):locs)

addFile :: String -> Int -> NodeZipper -> NodeZipper
addFile name size (Dir dirName children, locs) =
  (Dir dirName (Map.insert name (File name size) children), locs)

addDir :: String -> NodeZipper -> NodeZipper
addDir name (Dir dirName children, locs) =
  let newChildren = Map.insert name (Dir name mempty) children
  in (Dir dirName newChildren, locs)

buildFilesystem :: [String] -> Node
buildFilesystem = fst . top . foldl (flip interpretLine) (zipper rootNode)

interpretLine :: String -> NodeZipper -> NodeZipper
interpretLine = \case
  "$ ls" -> id
  "$ cd .." -> nodeUp
  ('$':' ':'c':'d':' ':dir) -> nodeDown dir
  ('d':'i':'r':' ':dir) -> addDir dir
  cmd -> let [size, name] = splitOn " " cmd in addFile name (read size)

nodeSize :: Node -> Int
nodeSize (File _ size)    = size
nodeSize (Dir _ children) = sum $ nodeSize <$> children

minDirSizes :: Int -> Node -> [Int]
minDirSizes _ (File _ _) = []
minDirSizes minSize dir@(Dir _ children) =
  (guard (nodeSize dir >= minSize) $> nodeSize dir)
    ++ concatMap (minDirSizes minSize) children

deleteDirSize :: Node -> Int
deleteDirSize node =
  let deleteSpace = 30000000 - (70000000 - nodeSize node)
  in minimum $ minDirSizes deleteSpace node
