module Rob.Actions.New (main) where

import Rob.Logger (err, warning, success)
import Rob.Config (get, errorNoTemplatesAvailable)
import Rob.Types (Config(..))
import Rob.Project (getTemplatePathByName, getTemplateName, createFilesFromTemplate)
import Rob.UserMessages (noTemplatesAvailable, choseATemplate, noTemplateSelected, tryAddingATemplate, projectSuccessfullyCreated, emptyString)
import Rob.Questionnaire (run)

import System.Exit
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
    responses <- run path
    putStrLn emptyString
    createFilesFromTemplate path responses
    success projectSuccessfullyCreated
    exitSuccess
  else do
    err noTemplateSelected
    exitFailure
