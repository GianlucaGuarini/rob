module Rob.Actions.Add (main) where

import Rob.Logger (err, success)
import Rob.Config (get, addTemplate)
import Rob.Types (Template(..))
import Rob.Project (hasPathQuestionnaire)
import Rob.UserMessages (projectPathDoesNotExist, projectQuestionnaireMissing, projectAdded)

import System.Exit
import System.Directory
import System.FilePath
import qualified Data.Yaml as Yaml

main :: String -> FilePath -> IO()
main name path = do
  hasProjectPath <- doesPathExist path
  if hasProjectPath then do
    hasQuestionnaire <- hasPathQuestionnaire path
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
