{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

module Day07 where

import           Util

import           Control.Lens
import           Control.Monad.State
import           Data.FileEmbed      (embedStringFile)
import           Data.Foldable       (fold)
import           Data.List           (nub, sort, (\\))
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe, listToMaybe, mapMaybe)
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Text.Regex

input :: [String]
input = lines $(embedStringFile "input/day07.txt")

extractInputRegex :: Regex
extractInputRegex =
    mkRegex "Step ([A-Z]) must be finished before step ([A-Z]) can begin."

requirements :: [String] -> Map Char (Set Char)
requirements =
    foldr (M.unionWith mappend) M.empty .
    fromMaybe (error "Error in reading") .
    traverse
        (fmap (\[a, b] -> M.singleton (head b) (S.singleton $ head a)) .
         matchRegex extractInputRegex)

jobsAvailable :: Map Char (Set Char) -> [Char] -> [Char]
jobsAvailable r c =
    let tasksToDo = (nub $ sort (M.keys r ++ S.toList (fold r))) \\ c
        canDoJob j =
            S.null (fromMaybe S.empty (M.lookup j r) S.\\ (S.fromList c))
        nextTask = filter canDoJob tasksToDo
     in nextTask

nextStep :: Map Char (Set Char) -> [Char] -> Maybe Char
nextStep r = listToMaybe . jobsAvailable r

resA :: [String] -> [Char]
resA i =
    let helper r c =
            case nextStep r c of
                Just j  -> j : helper r (j : c)
                Nothing -> []
     in helper (requirements i) []

data Worker
    = WaitingForInput
    | WorkingOn Char
                Int
    deriving (Eq, Show)

currentJob :: Worker -> Maybe Char
currentJob (WorkingOn c _) = Just c
currentJob _               = Nothing

timeToCompleteTask :: Char -> Int
timeToCompleteTask c = fromEnum c - fromEnum 'A' + 60

data WorkState = WorkState
    { _completedJobs :: Set Char
    , _currentJobs   :: Set Char
    } deriving (Eq, Show)

makeLenses ''WorkState

updateWorker ::
       Map Char (Set Char) -> WorkState -> Worker -> (WorkState, Worker)
updateWorker r ws (WorkingOn c n)
    | n + 1 > timeToCompleteTask c =
        updateWorker r (ws & completedJobs %~ S.insert c) WaitingForInput
    | otherwise = (ws, WorkingOn c (n + 1))
updateWorker r ws WaitingForInput =
    case jobsAvailable r (S.toList $ ws ^. completedJobs) \\
         S.toList (ws ^. currentJobs) of
        []    -> (ws, WaitingForInput)
        (j:_) -> (ws & currentJobs %~ S.insert j, WorkingOn j 0)

step :: Map Char (Set Char) -> WorkState -> [Worker] -> (WorkState, [Worker])
step r wState [] = (wState, [])
step r wState (w:ws) =
    let (wState', w') = updateWorker r wState w
     in over _2 (w' :) (step r wState' ws)

resB :: [String] -> Int -> [(WorkState, [Worker])]
resB i n =
    takeWhile
        (\(workState, workers) ->
             workState ^. completedJobs /= S.fromList ['A' .. 'Z']) $
    iterate
        (uncurry $ step (requirements i))
        (WorkState S.empty S.empty, replicate n WaitingForInput)

result :: DayResult String Int
result = DayResult {partA = resA input, partB = length (resB input 5) - 1}
