module Rob.Actions.Remove (main) where

import Rob.Logger (err, warning, success)
import Rob.Config (get, errorNoTemplatesAvailable, deleteTemplate)
import Rob.Types (Config(..))
import Rob.Project (getTemplateName)
import Rob.UserMessages (
    noTemplatesAvailable,
    choseATemplateToDelete,
    noTemplateSelected,
    tryAddingATemplate,
    emptyString
  )

import System.Exit
import FortyTwo (select)

main :: IO ()
main = do
  config <- get
  nukeTemplate config

-- Remove a project from the available templates
nukeTemplate :: Config -> IO ()
nukeTemplate (Config []) = errorNoTemplatesAvailable
nukeTemplate (Config templates) = do
  templateName <- select choseATemplateToDelete $ map getTemplateName templates
  putStrLn emptyString
  if (not . null) templateName then do
    deleteTemplate (Config templates) templateName
    exitSuccess
  else do
    err noTemplateSelected
    exitFailure
