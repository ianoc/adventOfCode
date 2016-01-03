module AdventOfCode.Command.Day3 (
  curCommand
) where

import AdventOfCode
import Options.Applicative
import AdventOfCode.Command.Utils
import qualified AdventOfCode.Day3 as Day3

data CmdArgs = CmdArgs {}

curCommand :: Command
curCommand = Command { commandName = "day3"
                      , commandDesc = "Run day 3 code"
                      , commandParser = cmdParser
                      , commandAction = cmdAction }

cmdParser :: Parser CmdArgs
cmdParser = pure CmdArgs

cmdAction :: CmdArgs -> IO ()
cmdAction sargs = do
  inputLines <- readInput
  let flattenedInput = flattenStr " " inputLines
  let housesVisited = Day3.housesVisited flattenedInput
  putStr "Santa visited: "
  putStrLn $ show housesVisited

  let housesVisitedWithRobo = Day3.housesVisitedWithRoboSanta flattenedInput
  putStr "Santa and Robo visited: "
  putStrLn $ show housesVisitedWithRobo
