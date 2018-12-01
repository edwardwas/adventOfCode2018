{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Day01
import           Util

import           Control.Monad       (forM_, guard)
import           Data.Bifunctor
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Options.Applicative

data RunOptions = RunOptions
    { daysToRun :: [Int]
    } deriving (Eq, Show)

parseRunOptions :: Parser RunOptions
parseRunOptions =
    let readDay = do
            n <- auto
            guard $ n `M.member` days
            return n
     in do daysToRun <-
               fromMaybe (M.keys days) <$>
               optional (some $ option readDay (short 'd' <> long "day"))
           pure $ RunOptions {..}

days :: Map Int (DayResult String String)
days = M.fromList [(1,first show $ second show $ Day01.result)]

runWithOptions :: RunOptions -> IO ()
runWithOptions RunOptions{..} =
  forM_ daysToRun $ \i -> case M.lookup i days of
    Just r  -> printDayResult i r
    Nothing -> putStrLn ("Unable to find day " <> show i)

main :: IO ()
main = execParser (info (helper <*> parseRunOptions) fullDesc) >>= runWithOptions
