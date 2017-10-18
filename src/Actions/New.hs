{-# LANGUAGE NamedFieldPuns #-}

module Actions.New (main) where

import System.Exit
import Questionnaire (Questionnaire)
import System.Console.Questioner (prompt)
import UserMessages (choseATemplate, noTemplatesAvailable, tryAddingATemplate)
import Logger (err, warning)
import Config (get, Config(..), Template(..))

main :: IO ()
main = do
  availableTemplates <- get
  createNewProject availableTemplates

getTemplateName :: Template -> String
getTemplateName Template { name } = name

getTemplatePath :: Template -> FilePath
getTemplatePath Template { path } = path

createNewProject :: Config -> IO ()
createNewProject (Config []) = do
  err noTemplatesAvailable
  warning tryAddingATemplate
  exitFailure
createNewProject (Config templates) = do
  templatePath <- prompt (choseATemplate, map getTemplateName templates) :: IO [String]
  return ()
