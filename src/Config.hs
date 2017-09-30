{-# LANGUAGE DeriveGeneric #-}

module Config (get, set) where

import qualified Package
import qualified Logger

import Data.Maybe
import qualified Data.Yaml as Yaml
import qualified Data.ByteString.Char8 as Char
import qualified GHC.Generics as GHC
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

-- | Template name + path
data Template = Template {
  name :: String,
  path :: FilePath
} deriving (GHC.Generic, Show)

instance Yaml.FromJSON Template
instance Yaml.ToJSON Template

-- | Config file struct
data Config = Config {
  version :: String,
  templates :: [Template]
} deriving (GHC.Generic, Show)

instance Yaml.FromJSON Config
instance Yaml.ToJSON Config

-- | Get the config file name
configFileName :: String
configFileName = ".rob"

-- | Get the whole path to the config file
configFilePath :: IO FilePath
configFilePath = do
  home <- Directory.getHomeDirectory
  return $ FilePath.joinPath [home, configFileName]

set :: Config -> IO Config
set config = do
  configFilePath >>= \path -> Yaml.encodeFile path config
  return config

-- | Get the current Config file Data
-- | If it doesn't exist it will create a new one
get :: IO Config
get = do
         path <- configFilePath
         hasConfigPath <- Directory.doesFileExist path
         if hasConfigPath
           then do
             Logger.success $ unwords [path, "was found!"]
             config <- Yaml.decodeFile path
             return $ fromJust config
           else do
             Logger.warning $ unwords [
                 "No",
                 configFileName,
                 "file was found in your $HOME path"
               ]
             Logger.flatten Logger.info [
                 "Creating a new config file in:",
                 path
               ]
             -- return an empty Config object and write it in the home directory
             set $ Config Package.version []
