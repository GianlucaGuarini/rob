module Rob.Project where

import Rob.Types (Template(..))
import Rob.Logger (err, warning)

import Data.Yaml (Value)
import Control.Monad (forM_)
import Data.Text (Text)
import Text.EDE (render, eitherParse, fromPairs)
import System.Directory.PathWalk (pathWalk)
import System.Directory (doesFileExist)
import System.FilePath (joinPath)

-- Get only the template name
getTemplateName :: Template -> String
getTemplateName (Template name _) = name

-- Get only the template path
getTemplatePath :: Template -> FilePath
getTemplatePath (Template _ path) = path

-- Get the template name by its path
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

-- | Check if the path contains the questionnaire file
hasPathQuestionnaire :: FilePath -> IO Bool
hasPathQuestionnaire = doesFileExist . questionnaireFileByPath

-- | Get the questionnaire file by project path
questionnaireFileByPath :: FilePath -> FilePath
questionnaireFileByPath path = joinPath [path, "project.yml"]

-- Create project files using the template and the user response received in the current directory
createFilesFromTemplate :: FilePath -> [(Text, Value)] -> IO()
createFilesFromTemplate path responses =
  pathWalk path $ \root dirs files ->
  forM_ files $ \file ->
    putStrLn $ joinPath [root, file]

  -- case eitherParse $ B.pack "The environement is\n {{ environement }}" of
  --   Right t ->
  --     print $ show $ render t (fromPairs responses)
  --   Left e -> exitFailure
