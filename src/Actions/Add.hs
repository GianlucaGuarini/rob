module Actions.Add (add) where

import UserMessages (projectPathDoesNotExist)
import Logger (err)
import System.Directory
import System.FilePath
import Config (get)

add :: String -> FilePath -> IO()
add name path = do
  hasProjectPath <- doesPathExist path
  if hasProjectPath then
    print "Path exists!"
  else
    err $ projectPathDoesNotExist path
