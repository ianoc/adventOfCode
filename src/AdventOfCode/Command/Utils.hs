module AdventOfCode.Command.Utils (
  tabColumnarize,
  evenColumnarize,
  readInput,
  flattenStr
) where

import Data.List(intercalate, transpose)
import qualified Data.Text as Text
import qualified Turtle as T
import qualified System.IO as IO
import Control.DeepSeq

tabColumnarize :: [[String]] -> [String]
tabColumnarize = map (intercalate "\t")


{-| Pad the i^th string with enough space to make columns line up
  >>> evenColumnarize [["yo", "man"], ["foo", "bar"], ["bazbaz", "baby"]]
  ["yo     man ","foo    bar ","bazbaz baby"]
-}
evenColumnarize :: [[String]] -> [String]
evenColumnarize rows = let
  columns = transpose rows
  widths = map (\c -> maximum (map length c)) columns
  padTo t str = let
    sz = length str
    pads = t - sz
    tail' = replicate pads ' '
    in str ++ tail'
  resCol = map (\(w, c) -> map (padTo w) c) (zip widths columns)
  in map (intercalate " ") (transpose resCol)




mergeUntilTwoBlankLines :: (() -> IO (Maybe String)) -> IO [String]
mergeUntilTwoBlankLines fn = do
    rd <- fn ()
    processFunc [] rd
  where processFunc ("" : existing) (Just "") = return existing
        processFunc existing (Just nxt) = do
          rd <- fn ()
          processFunc (nxt : existing) rd
        processFunc existing Nothing = return existing

readInput :: IO [String]
readInput = do
    IO.hSetBuffering IO.stdin IO.NoBuffering
    res <- mergeUntilTwoBlankLines (\_ -> readFn)
    res `deepseq` IO.hSetBuffering IO.stdin IO.LineBuffering
    return res
  where readFn = do
          readLineMaybe <- T.readline
          return $ fmap Text.unpack readLineMaybe


flattenStr :: String -> [String] -> String
flattenStr separator (x:xs) = x ++ separator ++ (flattenStr separator xs)
flattenStr _ [] = []