{-# LANGUAGE RecordWildCards #-}

module Main ( main ) where

import Tasks
import System.Console.CmdArgs

main = parse =<< cmdArgsRun mode

parse :: Task -> IO ()
parse opts@Add {..} = print "Add new template"
parse opts@New = print "Create a new project"
