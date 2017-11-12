module Rob.Actions.List (main) where

import Rob.Logger (info)
import Rob.UserMessages (availableTemplates, emptyString)
import Rob.Config (get, errorNoTemplatesAvailable)
import Rob.Types(Config(..))

import System.Exit

main :: IO()
main = do
  config <- get
  putStrLn emptyString
  info availableTemplates
  putStrLn emptyString
  listTemplates config
  exitSuccess

-- | List all the templates as string

listTemplates :: Config -> IO ()
listTemplates (Config []) = errorNoTemplatesAvailable;
listTemplates (Config templates) = putStrLn $ unlines $ map show templates
