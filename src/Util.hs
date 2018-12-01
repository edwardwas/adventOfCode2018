{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}

module Util where

data DayResult a b = DayResult
    { partA :: a
    , partB :: b
    } deriving (Functor,Eq,Show)

printDayResult :: (Show a, Show b) => Int -> DayResult a b -> IO ()
printDayResult dayNum DayResult{..} = do
  putStrLn ("Day " <> show dayNum <> ":")
  putStr "\tPart A: "
  print partA
  putStr "\tPart B: "
  print partB

