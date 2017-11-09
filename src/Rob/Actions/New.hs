{-# LANGUAGE NamedFieldPuns #-}

module Rob.Actions.New (main) where

import Rob.Questionnaire (run)
import Rob.UserMessages (choseATemplate, noTemplatesAvailable, noTemplateSelected, tryAddingATemplate)
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

getTemplatePathByName :: [Template] -> String -> FilePath
getTemplatePathByName _ "" = ""
getTemplatePathByName (x:xs) name =
  if templateName == name then
    templatePath
  else
    getTemplatePathByName xs name
    where
      templateName = getTemplateName x
      templatePath = getTemplatePath x

createNewProject :: Config -> IO ()
createNewProject (Config []) = do
  err noTemplatesAvailable
  warning tryAddingATemplate
  exitFailure
createNewProject (Config templates) = do
  templateName <- select choseATemplate $ map getTemplateName templates
  createProject $ getTemplatePathByName templates templateName

createProject :: FilePath -> IO()
createProject path =
  if not $ null path then
    print "run questionnaire"
  else do
    putStrLn ""
    err noTemplateSelected
