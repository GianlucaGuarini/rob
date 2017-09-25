module Config ( r ) where

import Paths_rob (version)
import Data.Version (showVersion)

import Data.Text (Text)
import qualified Text.Karver
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Yaml

import System.Directory ( getHomeDirectory, doesFileExist )

data Config = Config {
  version :: String,
  templates :: [String]
}

robFile :: String
robFile = ".rob"

robVersion :: String
robVersion = showVersion version

defaults :: HashMap Text Value
defaults = H.fromList $ [ ("version", Literal robVersion) ]

createRobFile :: IO()
createRobFile p = do
  let robFile = karver.renderTemplate defaults "version {{version}}\n- templates:"
  T.writeFile p robFile

read :: IO (Maybe Config)
read | hasRobFile = do
        file <- yaml.decodeFile pathToRobFile :: IO (Maybe Config)
        return file
     | otherwise = do
        createRobFile pathToRobFile
        return read
      where
        home = do
          path <- getHomeDirectory
          return path
        pathToRobFile = home + "/" + robFile
        hasRobFile = doesFileExist pathToRobFile

