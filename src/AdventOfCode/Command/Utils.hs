module AdventOfCode.Command.Utils (
  tabColumnarize,
  evenColumnarize,
  readInput,
  flattenStr
) where

import Data.List(intercalate, transpose)
import AdventOfCode
import qualified Data.Int as Ints
import qualified Data.Text as Text
import qualified Turtle as T

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




mergeUntilNothing :: (() -> IO (Maybe String)) -> IO [String]
mergeUntilNothing fn = do
    rd <- fn ()
    processFunc [] rd
  where processFunc ("" : existing) (Just "") = return existing
        processFunc existing (Just nxt) = do
          rd <- fn ()
          processFunc (nxt : existing) rd
        processFunc existing Nothing = return existing

readInput :: IO [String]
readInput = mergeUntilNothing (\_ -> readFn)
  where readFn = do
          readLineMaybe <- T.readline
          return $ fmap Text.unpack readLineMaybe


flattenStr :: String -> [String] -> String
flattenStr separator (x:xs) = x ++ separator ++ (flattenStr separator xs)
flattenStr _ [] = []