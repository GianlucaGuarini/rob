module Main where

import System.Environment
import System.Exit
import System.Console.CmdArgs (cmdArgsRun)
import System.Console.CmdArgs.Explicit(helpText, HelpFormat(..))
import Rob.Types (Task(..), mode)
import qualified Rob.Actions.Add
import qualified Rob.Actions.New
import qualified Rob.Actions.List

main :: IO()
main = do
  args <- getArgs
  if null args then exitWithHelp
  else parse =<< cmdArgsRun mode

parse :: Task -> IO ()
parse Add  {name = n, path = p} = Actions.Add.main n p
parse New = Actions.New.main
parse List = Actions.List.main

exitWithHelp :: IO a
exitWithHelp = do
  putStr $ show $ helpText [] HelpFormatOne mode
  exitSuccess
