module Actions.Add (main) where

import System.Exit
import System.Directory
import System.FilePath
import UserMessages (projectPathDoesNotExist, projectQuestionnaireMissing, projectAdded)
import Logger (err, success)
import qualified Data.Yaml as Yaml
import Config (get, addTemplate, Template)

main :: String -> FilePath -> IO()
main name path = do
  hasProjectPath <- doesPathExist path
  if hasProjectPath then do
    hasQuestionnaire <- doesFileExist projectQuestionnairePath
    if hasQuestionnaire then do
      config <- get
      addTemplate config name path
      success $ projectAdded name
      exitSuccess
    else do
      err $ projectQuestionnaireMissing path
      exitFailure
  else do
    err $ projectPathDoesNotExist path
    exitFailure
  where
    projectQuestionnairePath = joinPath [path, "project.yml"]
