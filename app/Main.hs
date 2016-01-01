module Main where


import Control.Monad(sequence_)
import AdventOfCode
import AdventOfCode.Command.All(allCommands)


main :: IO ()
main = do
  toAction allCommands