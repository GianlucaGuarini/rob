{-# LANGUAGE NamedFieldPuns #-}

module Actions.List (main) where


import UserMessages (availableTemplates)
import Config (get, Config(..), Template(..))

emptyString :: String
emptyString = ""

main :: IO()
main = do
  config <- get
  putStrLn emptyString
  putStrLn availableTemplates
  putStrLn emptyString
  putStrLn $ unlines $ listTemplates config

listTemplates :: Config -> [String]
listTemplates Config { templates } = map templateToString templates

templateToString :: Template -> String
templateToString Template { name, path } = unlines [
    "- Name: " ++ name,
    "  Path: " ++ path
  ]
