{-# LANGUAGE RecordWildCards #-}

module Main where
  
import Tasks
import System.Console.CmdArgs

main = parse =<< cmdArgsRun mode

parse :: Task -> IO ()
parse Add {..} = print "Add new template"
parse New = print "Create a new project"
