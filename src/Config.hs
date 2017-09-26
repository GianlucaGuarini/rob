module Config (get) where

import qualified Paths_rob (version)
import Data.Version (showVersion)

import System.Directory ( getHomeDirectory, doesFileExist )
import System.FilePath ( joinPath, FilePath)

-- | Confi file struct
data Config = Config {
  version :: String,
  templates :: [(String, String)]
}

-- | Get the config file name
configFileName :: String
configFileName = ".rob"

-- | Return the Package version (stored in the .cabal file)
packageVersion :: String
packageVersion = showVersion Paths_rob.version

-- | Get the whole path to the config file
configFilePath :: String -> FilePath
configFilePath base = joinPath [base, configFileName]

-- | Get the current Config file Data
-- | If it doesn't exist it will create a new one 
get :: IO ()
get = do
        print $ "Version:" ++ packageVersion
        home <- getHomeDirectory
        hasRobFile <- doesFileExist $ configFilePath home
        if hasRobFile
          then
            print "has file"
          else
            print "must create"
