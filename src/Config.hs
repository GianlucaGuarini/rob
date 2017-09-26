{-# LANGUAGE OverloadedStrings #-}

module Config (get) where

import qualified Paths_rob (version)
import Data.Version (showVersion)

import System.Directory ( getHomeDirectory, doesFileExist )
import System.FilePath ( joinPath, FilePath)

import Text.Karver
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.IO as TI

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

-- | Get the default config file data
defaultConfigData :: HashMap T.Text Value
defaultConfigData = H.fromList [ ("version", Literal $ T.pack packageVersion) ]

-- | Default config file template
defaultConfigTemplate :: T.Text
defaultConfigTemplate = T.pack $ unlines [
    "version: {{ version }}",
    "templates:"
  ]

-- | Render the default config file
renderDefaultConfiFile :: String
renderDefaultConfiFile = T.unpack $ renderTemplate defaultConfigData defaultConfigTemplate

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
            print renderDefaultConfiFile
