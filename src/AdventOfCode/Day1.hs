module AdventOfCode.Day1 (
    whichFloor
    ,indexTillFloor
    ) where

import Data.List(nub)

data HistoryUrl = HistoryUrl String deriving (Show, Eq, Ord)

data AppUrl = AppUrl String deriving (Show, Eq, Ord)

data Server = Server { serverName :: String, appUrl :: AppUrl, historyUrl :: HistoryUrl } deriving (Show, Eq, Ord)


whichFloor :: String -> Int
whichFloor input = fFloor 0 input
  where
      fFloor curFlr [] = curFlr
      fFloor curFlr (x:xs)
        | x == '('  = fFloor (curFlr + 1) xs
        | x == ')'  = fFloor (curFlr - 1) xs
        | otherwise = fFloor curFlr xs

indexTillFloor :: Int -> String -> Int
indexTillFloor targetFloor input = goToTarget 0 0 input
  where
      goToTarget index curFlr []
        | curFlr == targetFloor = index
        | otherwise             = -1
      goToTarget index curFlr (x:xs)
        | curFlr == targetFloor = index
        | x == '('  = goToTarget (index + 1) (curFlr + 1) xs
        | x == ')'  = goToTarget (index + 1) (curFlr - 1) xs
        | otherwise = goToTarget index curFlr xs

