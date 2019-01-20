module Rob.Config where

import qualified Rob.Package as Package
import Rob.Logger (err, warning, success, info, flatten)
import Rob.UserMessages (configFileFound, noConfigFileFound, configFileCreated, noTemplatesAvailable, tryAddingATemplate)
import Rob.Types (Config(..), Template(..))

import Data.Maybe
import System.Exit
import System.FilePath (joinPath)
import System.Directory (getHomeDirectory, doesFileExist)
import Data.Yaml (encodeFile, decodeFileEither)

-- | Get the config file name
configFileName :: String
configFileName = "." ++ Package.name

-- | Get the whole path to the config file
configFilePath :: IO FilePath
configFilePath = do
  home <- getHomeDirectory
  return $ joinPath [home, configFileName]

-- | Write the config file and return it
writeConfig :: Config -> IO Config
writeConfig config = do
  configFilePath >>= \path -> encodeFile path config
  return config

-- | Read the config file and return it
readConfig :: FilePath -> IO Config
readConfig path = either (error . show) id <$> decodeFileEither path

-- | Get the current Config file Data
-- | If it doesn't exist it will create a new one
get :: IO Config
get = do
  path <- configFilePath
  hasConfigPath <- doesFileExist path
  if hasConfigPath
    then do
      success $ configFileFound path
      readConfig path
    else do
      warning $ noConfigFileFound configFileName
      flatten info $ configFileCreated path
      -- return an empty Config object and write it in the home directory
      writeConfig $ Config []

-- | Dispatch the no templates available error
errorNoTemplatesAvailable :: IO ()
errorNoTemplatesAvailable = do
  err noTemplatesAvailable
  warning tryAddingATemplate
  exitFailure

-- | Add a new template to the config object and write it
addTemplate :: Config -> String -> String -> IO Config
addTemplate (Config templates) name path = writeConfig $ Config newTemplates
  where
    newTemplate = Template name path
    newTemplates = if newTemplate `elem` templates then
        map (\t -> if t == newTemplate then newTemplate else t) templates
      else
        templates ++ [newTemplate]

-- | Delete a template from the list of templates
deleteTemplate :: Config -> String -> IO Config
deleteTemplate (Config templates) nameToRemove = writeConfig $ Config newTemplates
  where
    newTemplates = (\(Template name _) -> name /= nameToRemove) `filter` templates
