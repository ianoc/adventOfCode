{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode (
    -- * Modules
    Command(..),
    toAction,
    failOnLeft,
    failOnNothing,
    err,
    ) where

import Data.Typeable
import Options.Applicative
import Control.Exception

data FailOnLeftException = FailOnLeftException String deriving (Show, Typeable)
instance Exception FailOnLeftException


failOnLeft :: Either String b -> b
failOnLeft (Right e) = e
failOnLeft (Left str')  = throw $ FailOnLeftException str'

data FailOnNothingException = FailOnNothingException String deriving (Show, Typeable)
instance Exception FailOnNothingException

failOnNothing :: String -> Maybe a -> a
failOnNothing _      (Just e) = e
failOnNothing errMsg Nothing  = throw $ FailOnNothingException errMsg

data ForcedException = ForcedException String deriving (Show, Typeable)
instance Exception ForcedException

err :: String -> a
err msg = throw $ ForcedException msg

data FailOnBadConfig = FailOnBadConfig String deriving (Show, Typeable)
instance Exception FailOnBadConfig

{-
 This is the struture we fit each subcommand into
 -}
data Command = forall a . Command {
  commandName :: String,
  commandDesc :: String,
  commandParser :: Parser a,
  commandAction :: a -> IO ()
}

{-
  Prepares the subcommand
-}
subcom :: Command -> Mod CommandFields (IO ())
subcom Command { commandName = name', commandDesc = desc, commandParser = parser, commandAction = act } = let
  toAct = act
  actionParser = helper <*> (toAct <$> parser)
  in command name' (info actionParser (progDesc desc <> header desc))

{-
  Called by the main function to run one of the commands
-}
toAction :: [Command] -> IO ()
toAction commands = let
  subcommands = map subcom commands
  subc = subparser (mconcat subcommands)
  in do
    action' <- execParser (info (helper <*> subc) (fullDesc <> header "jt: a command line job tracker tool"))
    action'
