module Rob.Config where

import qualified Rob.Package as Package
import Rob.Logger (err, warning, success, info, flatten)
import Rob.UserMessages (configFileFound, noConfigFileFound, configFileCreated, noTemplatesAvailable, tryAddingATemplate)
import Rob.Types (Config(..), Template(..))

import Data.Maybe
import System.Exit
import System.FilePath (joinPath)
import System.Directory (getHomeDirectory, doesFileExist)
import Data.Yaml (encodeFile, decodeFile)

-- | Get the config file name
configFileName :: String
configFileName = "." ++ Package.name

-- | Get the whole path to the config file
configFilePath :: IO FilePath
configFilePath = do
  home <- getHomeDirectory
  return $ joinPath [home, configFileName]

-- | Write the config file and return it
write :: Config -> IO Config
write config = do
  configFilePath >>= \path -> encodeFile path config
  return config

-- | Get the current Config file Data
-- | If it doesn't exist it will create a new one
get :: IO Config
get = do
  path <- configFilePath
  hasConfigPath <- doesFileExist path
  if hasConfigPath
    then do
      success $ configFileFound path
      config <- decodeFile path
      return $ fromJust config
    else do
      warning $ noConfigFileFound configFileName
      flatten info $ configFileCreated path
      -- return an empty Config object and write it in the home directory
      write $ Config []

-- | Dispatch the no templates available error
errorNoTemplatesAvailable :: IO ()
errorNoTemplatesAvailable = do
  err noTemplatesAvailable
  warning tryAddingATemplate
  exitFailure

-- | Add a new template to the config object and write it
addTemplate :: Config -> String -> String -> IO Config
addTemplate (Config templates) name path = write $ Config newTemplates
  where
    newTemplate = Template name path
    newTemplates = if newTemplate `elem` templates then
        map (\t -> if t == newTemplate then newTemplate else t) templates
      else
        templates ++ [newTemplate]

-- | Delete a template from the list of templates
deleteTemplate :: Config -> String -> IO Config
deleteTemplate (Config templates) nameToRemove = write $ Config newTemplates
  where
    newTemplates = (\(Template name _) -> name /= nameToRemove) `filter` templates
