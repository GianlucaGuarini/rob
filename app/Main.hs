module Main where

import System.Environment
import System.Exit
import System.Console.CmdArgs (cmdArgsRun)
import System.Console.CmdArgs.Explicit(helpText, HelpFormat(..))
import Tasks (Task(Add, New), mode, name, path)
import Actions.Add (add)
import Actions.New (new)

main :: IO()
main = do
  args <- getArgs
  if null args then exitWithHelp
  else parse =<< cmdArgsRun mode

parse :: Task -> IO ()
parse Add  {name = n, path = p} = add n p
parse New = new

exitWithHelp :: IO a
exitWithHelp = do
  putStr $ show $ helpText [] HelpFormatAll mode
  exitSuccess
