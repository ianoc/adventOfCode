module AdventOfCode.Command.All(allCommands) where

import AdventOfCode(Command)
import AdventOfCode.Command.Day1(day1Command)
import qualified AdventOfCode.Command.Day2 as Day2
import qualified AdventOfCode.Command.Day3 as Day3
allCommands :: [Command]
allCommands = [day1Command, Day2.curCommand, Day3.curCommand]
