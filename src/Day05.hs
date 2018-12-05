{-# LANGUAGE TemplateHaskell #-}

module Day05 where

import           Util

import           Data.Char      (toUpper)
import           Data.FileEmbed (embedStringFile)

input :: String
input = head $ lines $ $(embedStringFile "input/day05.txt")

result :: DayResult Int Int
result =
    DayResult
        { partA = length $ removeAllReacting input
        , partB =
              minimum
                  [ length $
                  removeAllReacting $
                  filter (\i -> i /= c && i /= toUpper c) input
                  | c <- ['a' .. 'z']
                  ]
        }

removeAllReacting :: String -> String
removeAllReacting =
    let helper (y:ys) x
            | x /= y && toUpper x == toUpper y = ys
            | otherwise = x : y : ys
        helper [] x = [x]
     in foldl helper ""
