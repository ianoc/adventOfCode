module AdventOfCode.Command.Day5 (
  curCommand
) where

import AdventOfCode
import Options.Applicative
import AdventOfCode.Command.Utils
import qualified AdventOfCode.Day5 as Day5
import Control.Monad
data CmdArgs = CmdArgs {}

curCommand :: Command
curCommand = Command { commandName = "day5"
                      , commandDesc = "Run day 5 code"
                      , commandParser = cmdParser
                      , commandAction = cmdAction }

cmdParser :: Parser CmdArgs
cmdParser = pure CmdArgs

cmdAction :: CmdArgs -> IO ()
cmdAction sargs = do
  inputLines <- readInput
  let niceInputs = Day5.niceStrings inputLines
  putStr "Nice Lines: "
  putStrLn $ show (length niceInputs)

  let niceInputs2 = Day5.niceStrings2 inputLines
  putStr "Nice Lines2: "
  putStrLn $ show (length niceInputs2)
