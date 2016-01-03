module AdventOfCode.Command.Day4 (
  curCommand
) where

import AdventOfCode
import Options.Applicative
import AdventOfCode.Command.Utils
import qualified AdventOfCode.Day4 as Day4

data CmdArgs = CmdArgs { cmdASecretKey :: String , cmdALeadingZeros :: Maybe Int }

curCommand :: Command
curCommand = Command { commandName = "day4"
                      , commandDesc = "Run day 4 code"
                      , commandParser = cmdParser
                      , commandAction = cmdAction }

cmdParser :: Parser CmdArgs
cmdParser = let
  leadingZerosP = optional(option auto (long "leading-zeros" <> short 'z' <> metavar "LEADINGZEROS" <> help "leading zeros"))
  secretKeyP = strOption (long "secret-key" <> short 's' <> metavar "SECRETKEY" <> help "secretKey")
  in CmdArgs <$> secretKeyP <*> leadingZerosP :: Parser CmdArgs


cmdAction :: CmdArgs -> IO ()
cmdAction (CmdArgs secretKey (Just leadingZeros)) = do
  putStr "LowestPositive number visited: "
  putStrLn $ show (Day4.lowestPositiveNumber leadingZeros secretKey)
cmdAction (CmdArgs secretKey Nothing) = cmdAction $ CmdArgs secretKey (Just 5)