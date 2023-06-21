module Tree
(
  Tree(..),
  getSimbol,
  getLeft,
  getRight,
  isBase,
  buildTree
) where

data Tree = Empty 
  | Tree Tree String Tree 
  deriving (Show, Read, Eq)

isBase :: Tree -> Bool
isBase tree = getLeft tree == Empty && getRight tree == Empty

getLeft :: Tree -> Tree
getLeft (Tree getLeft _ _) = getLeft

getRight :: Tree -> Tree
getRight (Tree _ _ getRight) = getRight

getSimbol:: Tree -> String
getSimbol(Tree _ getSimbol_) = getSimbol

buildTree :: String -> Tree
buildTree str = read str :: Tree