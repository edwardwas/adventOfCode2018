{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Day08 where

import           Util

import           Control.Lens        (ix, to, (^?))
import           Control.Monad.State (State (..), evalState, get, put,
                                      replicateM)
import           Data.FileEmbed      (embedStringFile)
import           Data.Maybe          (fromMaybe)

nextNum :: State [Int] Int
nextNum = get >>= \(x:xs) -> x <$ put xs

input :: [Int]
input = map read $ words $(embedStringFile "input/day08.txt")

data Tree a =
    Tree [Tree a]
         [a]
    deriving (Eq, Show, Functor, Foldable, Traversable)

readTree :: State [Int] (Tree Int)
readTree = do
    numChildNodes <- nextNum
    numMetadata <- nextNum
    Tree <$> replicateM numChildNodes readTree <*>
        replicateM numMetadata nextNum

valueOfTree :: Tree Int -> Int
valueOfTree (Tree children meta)
    | null children = sum meta
    | otherwise =
        sum $
        map (\m -> fromMaybe 0 $ children ^? ix (m - 1) . to valueOfTree) meta

result :: DayResult Int Int
result =
    let tree = evalState readTree input
     in DayResult {partA = sum tree, partB = valueOfTree tree}
