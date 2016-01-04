module AdventOfCode.Command.Day6 (
  curCommand
) where

import AdventOfCode
import Options.Applicative
import AdventOfCode.Command.Utils
import qualified AdventOfCode.Day6 as Day6

data CmdArgs = CmdArgs {}

curCommand :: Command
curCommand = Command { commandName = "day6"
                      , commandDesc = "Run day 6 code"
                      , commandParser = cmdParser
                      , commandAction = cmdAction }

cmdParser :: Parser CmdArgs
cmdParser = pure CmdArgs

baseGrid :: a -> [[a]]
baseGrid a = [[a | _ <- [0..999]] | _ <- [0..999]]

cmdAction :: CmdArgs -> IO ()
cmdAction sargs = do
  inputLines <- readInput
  let flattenedInput = flattenStr " " inputLines
  let grid = baseGrid False
  let instructions = failOnLeft $ Day6.parseInstructions flattenedInput
  let updated = Day6.gridUpdates grid instructions
  putStr "Turned on after instructions: "
  putStrLn $ show (Day6.turnedOn updated)

  let lumenUpdated = Day6.gridUpdatesLumen (baseGrid 0) instructions
  putStr "With variable intensitiy instructions: "
  putStrLn $ show (Day6.totalBrightness lumenUpdated)


