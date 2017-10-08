{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Console.CmdArgs (cmdArgsRun)
import Tasks (Task(Add, New), mode)
import Actions.Add (add)
import Actions.New (new)

main = parse =<< cmdArgsRun mode

parse :: Task -> IO ()
parse Add {..} = add
parse New = new
