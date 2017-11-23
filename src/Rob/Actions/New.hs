module Rob.Actions.New (main) where

import Rob.Logger (err, success)
import Rob.Config (get, errorNoTemplatesAvailable)
import Rob.Types (Config(..))
import Rob.Questionnaire (run)
import Rob.Project (getTemplatePathByName, getTemplateName, createFilesFromTemplate)
import Rob.UserMessages (
    choseATemplate,
    noTemplateSelected,
    projectSuccessfullyCreated,
    projectPathDoesNotExist,
    emptyString
  )

import System.Exit (exitFailure, exitSuccess)
import System.Directory (doesPathExist)
import FortyTwo (select)

main :: IO ()
main = do
  config <- get
  createNewProject config

-- Create a new project if
createNewProject :: Config -> IO ()
createNewProject (Config []) = errorNoTemplatesAvailable
createNewProject (Config templates) = do
  templateName <- select choseATemplate $ map getTemplateName templates
  putStrLn emptyString
  if not . null $ templateName then do
    let path = getTemplatePathByName templates templateName
    hasProjectPath <- doesPathExist path
    if hasProjectPath then do
      responses <- run path
      putStrLn emptyString
      createFilesFromTemplate path responses
      success projectSuccessfullyCreated
      exitSuccess
    else do
      err $ projectPathDoesNotExist path
      exitFailure
  else do
    err noTemplateSelected
    exitFailure
