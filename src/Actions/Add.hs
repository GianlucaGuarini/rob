module Actions.Add (add) where

import System.Directory
import System.FilePath
import UserMessages (projectPathDoesNotExist, projectQuestionnaireMissing, projectAdded)
import Logger (err, success)
import qualified Data.Yaml as Yaml
import Config (get, addTemplate, Template)

add :: String -> FilePath -> IO()
add name path = do
  hasProjectPath <- doesPathExist path
  if hasProjectPath then do
    hasQuestionnaire <- doesFileExist projectQuestionnairePath
    if hasQuestionnaire then do
      config <- get
      addTemplate config name path
      success $ projectAdded name
    else
      err $ projectQuestionnaireMissing path
  else
    err $ projectPathDoesNotExist path
  where
    projectQuestionnairePath = joinPath [path, "project.yml"]
