{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Day01
import qualified Day02
import qualified Day03
import           Util

import           Control.Monad       (forM_, unless)
import           Data.Bifunctor
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Options.Applicative

days :: Map Int (DayResult String String)
days =
    M.fromList
        [ (1, bimap show show Day01.result)
        , (2, bimap show id Day02.result)
        , (3, bimap show show Day03.result)
        ]

data RunOptions = RunOptions
    { daysToRun :: [Int]
    } deriving (Eq, Show)

parseRunOptions :: Parser RunOptions
parseRunOptions =
    let readDay = do
            n <- auto
            unless
                (n `M.member` days)
                (fail (show n <> " is not a day that has been completed yet"))
            return n
     in do daysToRun <-
               fromMaybe (M.keys days) <$>
               optional (some $ option readDay (short 'd' <> long "day"))
           pure $ RunOptions {..}

runWithOptions :: RunOptions -> IO ()
runWithOptions RunOptions {..} =
    forM_ daysToRun $ \i ->
        case M.lookup i days of
            Just r  -> printDayResult i r
            Nothing -> putStrLn ("Unable to find day " <> show i)

main :: IO ()
main =
    execParser (info (helper <*> parseRunOptions) fullDesc) >>= runWithOptions
