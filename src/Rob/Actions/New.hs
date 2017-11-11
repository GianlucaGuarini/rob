module Rob.Actions.New (main) where

import Rob.Logger (err, warning)
import Rob.Config (get)
import Rob.Types (Config(..))
import Rob.Project (getTemplatePathByName, getTemplateName, createFilesFromTemplate)
import Rob.UserMessages (noTemplatesAvailable, choseATemplate, noTemplateSelected, tryAddingATemplate)
import Rob.Questionnaire (run)

import System.Exit
import FortyTwo (select)

main :: IO ()
main = do
  availableTemplates <- get
  createNewProject availableTemplates

-- Create a new project if
createNewProject :: Config -> IO ()
createNewProject (Config []) = do
  err noTemplatesAvailable
  warning tryAddingATemplate
  exitFailure
createNewProject (Config templates) = do
  templateName <- select choseATemplate $ map getTemplateName templates
  putStrLn ""
  if not . null $ templateName then do
    let path = getTemplatePathByName templates templateName
    responses <- run path
    createFilesFromTemplate path responses
  else
    err noTemplateSelected
