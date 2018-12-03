{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Day03 where

import           Util

import           Data.FileEmbed       (embedStringFile)
import qualified Data.Map             as M
import qualified Data.Set             as S
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void Text

parseInt :: Parser Int
parseInt = read <$> many digitChar

input :: [Text]
input = T.lines $(embedStringFile "input/day03.txt")

data Claim = Claim
    { offset :: (Int, Int)
    , size   :: (Int, Int)
    } deriving (Eq, Show)

pointsInClaim :: Claim -> [(Int, Int)]
pointsInClaim Claim {..} =
    let (ox, oy) = offset
        (sx, sy) = size
     in [(ox + dx, oy + dy) | dx <- [0 .. sx - 1], dy <- [0 .. sy - 1]]

parseInputLine :: Parser (Int, Claim)
parseInputLine = do
    identNum <- "#" *> parseInt
    offsetX <- " @ " *> parseInt
    offsetY <- "," *> parseInt
    width <- ": " *> parseInt
    height <- "x" *> parseInt
    return (identNum, Claim (offsetX, offsetY) (width, height))

claimMap :: [(Int, Claim)]
claimMap = either (error . show) id $ traverse (parse parseInputLine "") input

pointsWhichOverlap :: [(Int, Int)]
pointsWhichOverlap =
    map fst $
    filter (\(_, n) -> n > 1) $
    M.toList $
    foldr
        (M.unionWith (+))
        M.empty
        (map (M.fromList . map (, 1 :: Int) . pointsInClaim . snd) claimMap)

result :: DayResult Int Int
result =
    DayResult
        { partA = length $ pointsWhichOverlap
        , partB =
              let s = S.fromList pointsWhichOverlap
               in fst $
                  head $
                  filter
                      (\(_, c) ->
                           all (\p -> not $ S.member p s) $ pointsInClaim c) $
                  claimMap
        }
