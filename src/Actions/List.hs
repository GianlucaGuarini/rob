{-# LANGUAGE NamedFieldPuns #-}

module Actions.List (main) where

import System.Exit
import Logger (info)
import UserMessages (availableTemplates, empty)
import Config (get, Config(..), Template(..))

main :: IO()
main = do
  config <- get
  putStrLn empty
  info availableTemplates
  putStrLn empty
  putStrLn $ unlines $ listTemplates config
  exitSuccess

listTemplates :: Config -> [String]
listTemplates Config { templates } = map templateToString templates

templateToString :: Template -> String
templateToString Template { name, path } = unlines [
    "- Name: " ++ name,
    "  Path: " ++ path
  ]
