{-# LANGUAGE NamedFieldPuns #-}

module Rob.Actions.List (main) where

import Rob.Logger (info)
import Rob.UserMessages (availableTemplates, empty)
import Rob.Config (get, Config(..), Template(..))

import System.Exit

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
