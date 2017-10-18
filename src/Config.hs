{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Config where

import qualified Package
import qualified Logger
import UserMessages (configFileFound, noConfigFileFound, configFileCreated)

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
} deriving (GHC.Generic, Show, Ord, Eq)

instance Yaml.FromJSON Template
instance Yaml.ToJSON Template

-- | Config file struct
newtype Config = Config {
  templates :: [Template]
} deriving (GHC.Generic, Show, Ord, Eq)

instance Yaml.FromJSON Config
instance Yaml.ToJSON Config

-- | Get the config file name
configFileName :: String
configFileName = "." ++ Package.name

-- | Get the whole path to the config file
configFilePath :: IO FilePath
configFilePath = do
  home <- Directory.getHomeDirectory
  return $ FilePath.joinPath [home, configFileName]

-- | Write the config file and return it
write :: Config -> IO Config
write config = do
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
      Logger.success $ configFileFound path
      config <- Yaml.decodeFile path
      return $ fromJust config
    else do
      Logger.warning $ noConfigFileFound configFileName
      Logger.flatten Logger.info $ configFileCreated path
      -- return an empty Config object and write it in the home directory
      write $ Config []

-- | Add a new template to the config object and write it
addTemplate :: Config -> String -> String -> IO Config
addTemplate Config { templates } name path = write Config { templates = newTemplates }
  where
    newTemplates = templates ++ [Template { name, path }]
