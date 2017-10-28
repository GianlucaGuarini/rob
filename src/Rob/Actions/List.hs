{-# LANGUAGE NamedFieldPuns #-}

module Rob.Actions.List (main) where

import Rob.Logger (info)
import Rob.UserMessages (availableTemplates, emptyString)
import Rob.Config (get)
import Rob.Types(Config(..), Template(..))

import System.Exit

main :: IO()
main = do
  config <- get
  putStrLn emptyString
  info availableTemplates
  putStrLn emptyString
  putStrLn $ unlines $ listTemplates config
  exitSuccess

listTemplates :: Config -> [String]
listTemplates Config { templates } = map templateToString templates

templateToString :: Template -> String
templateToString Template { name, path } = unlines [
    "- Name: " ++ name,
    "  Path: " ++ path
  ]
