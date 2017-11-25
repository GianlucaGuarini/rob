module Rob.Actions.Remove (main) where

import Rob.Logger (err)
import Rob.Config (get, errorNoTemplatesAvailable, deleteTemplate)
import Rob.Types (Config(..))
import Rob.Project (getTemplateName)
import Rob.UserMessages (
    choseATemplateToDelete,
    noTemplateSelected,
    emptyString
  )

import System.Exit (exitFailure, exitSuccess)
import FortyTwo (select)

main :: IO ()
main = do
  config <- get
  nukeTemplate config

-- Remove a project template from the available ones
nukeTemplate :: Config -> IO ()
nukeTemplate (Config []) = errorNoTemplatesAvailable
nukeTemplate (Config templates) = do
  templateName <- select choseATemplateToDelete $ map getTemplateName templates
  putStrLn emptyString
  if (not . null) templateName then do
    _ <- deleteTemplate (Config templates) templateName
    exitSuccess
  else do
    err noTemplateSelected
    exitFailure
