{-# LANGUAGE NamedFieldPuns #-}

module Rob.Actions.New (main) where

import Rob.Questionnaire (Questionnaire)
import Rob.UserMessages (choseATemplate, noTemplatesAvailable, tryAddingATemplate)
import Rob.Config (get)
import Rob.Types (Config(..), Template(..))

import System.Exit
import FortyTwo (select)
import Rob.Logger (err, warning)

main :: IO ()
main = do
  availableTemplates <- get
  createNewProject availableTemplates

getTemplateName :: Template -> String
getTemplateName (Template name _) = name

getTemplatePath :: Template -> FilePath
getTemplatePath (Template _ path) = path

createNewProject :: Config -> IO ()
createNewProject (Config []) = do
  err noTemplatesAvailable
  warning tryAddingATemplate
  exitFailure
createNewProject (Config templates) = do
  templatePath <- select choseATemplate $ map getTemplateName templates
  return ()
