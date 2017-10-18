module Actions.New (main) where

import System.Exit
import Questionnaire (Questionnaire)
import UserMessages (choseATemplate, noTemplatesAvailable, tryAddingATemplate)
import Logger (err, warning)
import Config

main :: IO ()
main = do
  availableTemplates <- get
  createNewProject availableTemplates

createNewProject :: Config -> IO ()
createNewProject (Config []) = do
  err noTemplatesAvailable
  warning tryAddingATemplate
  exitFailure
createNewProject (Config templates) = print "we got some templates"
