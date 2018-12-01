{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Day01 where

import           Util           (DayResult (..))

import           Data.FileEmbed (embedStringFile)
import qualified Data.Set       as S
import qualified Data.Text      as T

parseInput :: String -> Integer
parseInput (c:cs)
  | c == '+' = read cs
  | c == '-' = negate $ read cs
  | otherwise = undefined
parseInput [] = undefined

input :: [Integer]
input = map (parseInput . T.unpack) $ T.lines $(embedStringFile "input/day01.txt")

firstRepeat :: Ord a => S.Set a -> [a] -> a
firstRepeat acc (a:as)
  | a `S.member` acc = a
  | otherwise = firstRepeat (S.insert a acc) as
firstRepeat _ _ = error "First repeat expects infinite input"

result :: DayResult Integer Integer
result =
    DayResult
        { partA = sum input
        , partB = firstRepeat S.empty $ scanl (+) 0 $ concat $ repeat input
        }
