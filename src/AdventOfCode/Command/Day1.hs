module AdventOfCode.Command.Day1 (
  day1Command
) where

import AdventOfCode
import Data.Maybe(fromMaybe)
import Options.Applicative
import AdventOfCode.Command.Utils
import Data.List(intercalate)
import Control.Monad.Reader
import Control.Monad
import Options.Applicative.Types
import AdventOfCode.Day1(whichFloor, indexTillFloor)

data Day1Args = Day1Args { findFloor :: Maybe Int
                         }

day1Command :: Command
day1Command = Command { commandName = "day1"
                      , commandDesc = "Run day 1 code"
                      , commandParser = day1Parser
                      , commandAction = day1Action }

day1Parser :: Parser Day1Args
day1Parser = let
  findFP = optional(option auto (long "find-floor" <> short 'f' <> metavar "FINDFLOOR" <> help "find index to reach floor"))
  in Day1Args <$> findFP :: Parser Day1Args

day1Action :: Day1Args -> IO ()
day1Action sargs = do
  inputLines <- readInput
  let flattenedInput = flattenStr " " inputLines
  let result = makeRes flattenedInput $ findFloor sargs
  putStrLn $ show result
  where makeRes flattenedInput Nothing         = whichFloor flattenedInput
        makeRes flattenedInput (Just targetFl) = indexTillFloor targetFl flattenedInput
