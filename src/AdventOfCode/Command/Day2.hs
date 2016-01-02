module AdventOfCode.Command.Day2 (
  curCommand
) where

import AdventOfCode
import Options.Applicative
import AdventOfCode.Command.Utils
import qualified AdventOfCode.Day2 as Day2

data CmdArgs = CmdArgs {}

curCommand :: Command
curCommand = Command { commandName = "day2"
                      , commandDesc = "Run day 2 code"
                      , commandParser = cmdParser
                      , commandAction = cmdAction }

cmdParser :: Parser CmdArgs
cmdParser = pure CmdArgs

data ResultsInfo = ResultsInfo {
          ribbonLength :: Int,
          wrappingPaper :: Int
        }

runCmd :: String -> ResultsInfo
runCmd input = ResultsInfo ribbonTot wrappingPaperTot
    where
      boxes = failOnLeft $ Day2.parseBoxes input
      wrappingPaperTot = sum $ fmap Day2.wrappingPaper boxes :: Int
      ribbonTot = sum $ fmap Day2.ribbonLength boxes :: Int

cmdAction :: CmdArgs -> IO ()
cmdAction sargs = do
  inputLines <- readInput
  let flattenedInput = flattenStr " " inputLines
  let (ResultsInfo ribbonLengthTot wrappingPaperTot) = runCmd flattenedInput
  putStr "WrappingPaper Requirement : "
  putStrLn $ show wrappingPaperTot
  putStr "Ribbon Requirement : "
  putStrLn $ show ribbonLengthTot


