{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}

module Util where

import           Data.Bifunctor

allPairs :: [a] -> [(a,a)]
allPairs []     = []
allPairs (a:as) = zip (repeat a) as ++ allPairs as

data DayResult a b = DayResult
    { partA :: a
    , partB :: b
    } deriving (Functor,Eq,Show)

instance Bifunctor DayResult where
  first f (DayResult pa pb) = DayResult (f pa) pb
  second = fmap

printDayResult :: Int -> DayResult String String -> IO ()
printDayResult dayNum DayResult{..} = do
  putStrLn ("Day " <> show dayNum <> ":")
  putStr "\tPart A: "
  putStrLn partA
  putStr "\tPart B: "
  putStrLn partB

