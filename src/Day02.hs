{-# LANGUAGE TemplateHaskell #-}

module Day02 where

import           Util

import           Control.Monad  (guard)
import           Data.FileEmbed (embedStringFile)
import qualified Data.Map       as M
import           Data.Maybe     (catMaybes)

input :: [String]
input = lines $(embedStringFile "input/day02.txt")

letterOccurances :: String -> M.Map Char Int
letterOccurances = foldr (\n m -> M.insertWith (+) n 1 m) M.empty

differences :: Eq a => [a] -> [a] -> Int
differences as bs = length $ filter not $ zipWith (==) as bs

result :: DayResult Int String
result =
    DayResult
        { partA =
              let lO = map letterOccurances input
               in length (filter (any (== 2)) lO) *
                  length (filter (any (== 3)) lO)
        , partB =
              head $
              map
                  (\(a, b) ->
                       catMaybes $ zipWith (\x y -> x <$ guard (x == y)) a b) $
              filter ((== 1) . uncurry differences) $ allPairs input
        }
