{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config (get) where

import Control.Lens hiding (element)
import System.Directory (getHomeDirectory, doesFileExist)
import System.FilePath (joinPath, FilePath)

import qualified Package
import qualified Logger as Log

import Text.Karver
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.IO as TI

-- | Template name + path
data Template = Template {
  _name :: String,
  _path :: FilePath
} deriving (Show, Read)

-- | Config file struct
data Config = Config {
  _version :: String,
  _templates :: [Template]
} deriving (Show, Read)

-- create the lenses for the custom data types
makeLenses ''Template
makeLenses ''Config

-- | Get the config file name
configFileName :: String
configFileName = ".rob"

-- | Get the whole path to the config file
configFilePath :: String -> FilePath
configFilePath base = joinPath [base, configFileName]

-- | Get the default config file data
defaultConfigData :: Config
defaultConfigData = Config Package.version []

-- | Get the default template data
defaultConfigTemplateData :: HashMap T.Text Value
defaultConfigTemplateData = H.fromList [
                              ("version", Literal $ T.pack $ defaultConfigData^.version)
                            ]

-- | Default config file template
defaultConfigTemplate :: T.Text
defaultConfigTemplate = T.pack $ unlines [
    "version: {{ version }}",
    "templates:"
  ]

-- | Render the default config file
renderDefaultConfiFile :: String
renderDefaultConfiFile = T.unpack $ renderTemplate defaultConfigTemplateData defaultConfigTemplate

-- | Get the current Config file Data
-- | If it doesn't exist it will create a new one
get :: IO Config
get = do
        home <- getHomeDirectory
        let configFile = configFilePath home
        hasRobFile <- doesFileExist configFile
        if hasRobFile
          then do
            Log.success $ unwords [configFile, "was found!"]
            -- TODO: read config file from the path
            return defaultConfigData
          else do
            Log.warning $ unwords [
                "No",
                configFileName,
                "file was found in your $HOME path"
              ]
            Log.flatten Log.info [
                "Creating a new config file in:",
                configFile
              ]
            writeFile configFile renderDefaultConfiFile
            -- return an empty Config object
            return defaultConfigData
