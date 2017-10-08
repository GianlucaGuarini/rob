module Actions.New (new) where

import System.Exit
import UserMessages (choseATemplate, noTemplatesAvailable, tryAddingATemplate)
import Questions (select, Option)
import Logger (err, warning)
import Config

new :: IO ()
new = do
  availableTemplates <- get
  createNewProject availableTemplates

createNewProject :: Config -> IO ()
createNewProject (Config []) = do
  err noTemplatesAvailable
  warning tryAddingATemplate
  exitFailure
createNewProject (Config templates) = print "we got some templates"
