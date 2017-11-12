{-# LANGUAGE OverloadedStrings #-}

module Rob.Project where

import Rob.Types (Template(..))
import Rob.Logger (err, warning, success)

import Data.Yaml (Value)
import Control.Monad (forM_, unless)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BS
import Data.Text.Lazy.Encoding (encodeUtf8)
import System.FilePath.Posix (makeRelative)
import Data.List (isSuffixOf, any)
import System.FilePath.Glob (match, compile, Pattern)
import Text.EDE (eitherRender, eitherParseFile, fromPairs)
import System.Directory.PathWalk (pathWalkInterruptible, WalkStatus(..))
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (joinPath, takeFileName)

-- Get only the template name
getTemplateName :: Template -> String
getTemplateName (Template name _) = name

-- Get only the template path
getTemplatePath :: Template -> FilePath
getTemplatePath (Template _ path) = path

-- Get the template name by its path
getTemplatePathByName :: [Template] -> String -> FilePath
getTemplatePathByName _ "" = ""
getTemplatePathByName (x:xs) name =
  if templateName == name then
    templatePath
  else
    getTemplatePathByName xs name
  where
    templateName = getTemplateName x
    templatePath = getTemplatePath x

-- | File where users can store all the project creation questions
projectDataFile :: String
projectDataFile = "project.yml"

-- | Check if the path contains the questionnaire file
hasPathQuestionnaire :: FilePath -> IO Bool
hasPathQuestionnaire = doesFileExist . questionnaireFileByPath

-- | Get the questionnaire file by project path
questionnaireFileByPath :: FilePath -> FilePath
questionnaireFileByPath path = joinPath [path, projectDataFile]

-- Create project files using the template and the user response received in the current directory
-- TODO: clean up this mess
createFilesFromTemplate :: FilePath -> [(Text, Value)] -> IO()
createFilesFromTemplate path responses =
  pathWalkInterruptible path $ \root dirs files -> do
    -- TODO: support also nested ignored files via recursion
    blacklist <- generateIgnoreBlacklist (map (\f -> joinPath [root, f]) blacklistFiles) knownIgnoredStuff
    let dirRelativePath = joinPath [makeRelative path root]
    if isInBlacklist (takeFileName dirRelativePath) blacklist then
      return StopRecursing
    else do
      createDirectoryIfMissing True dirRelativePath
      forM_ files $ \file ->
        unless (isInBlacklist file blacklist) $ do
          let fileAbsolutePath = joinPath [root, file]
          let fileRelativePath = joinPath [dirRelativePath, file]
          template <- eitherParseFile fileAbsolutePath
          case template of
            Right t ->
              case eitherRender t templateData of
                Right res -> BS.writeFile fileRelativePath (encodeUtf8 res)
                Left e -> fallback e fileAbsolutePath fileRelativePath
            Left e -> fallback e fileAbsolutePath fileRelativePath
      return Continue
    where
      blacklistFiles = [".gitignore", "svnignore.txt"]
      knownIgnoredStuff = globbify [".git", ".svn", projectDataFile]
      templateData = fromPairs responses
      fallback e inPath outPath = do
        err $ show e
        file <- readFile inPath
        writeFile outPath file

isInBlacklist :: FilePath -> [Pattern] -> Bool
isInBlacklist path = any (`match` path)

generateIgnoreBlacklist :: [FilePath] -> [Pattern] -> IO [Pattern]
generateIgnoreBlacklist [] blacklist = return blacklist
generateIgnoreBlacklist (x:xs) blacklist = ignoredStuff >>= \stuff -> generateIgnoreBlacklist xs (blacklist ++ stuff)
    where
      ignoredStuff = do
        hasFile <- doesFileExist x
        if hasFile then do
          file <- readFile x
          return $ globbify $ cleanList $ lines file
        else return []
      cleanList list = (\l -> (not . null) l && head l /= '#') `filter` list

globbify :: [String] -> [Pattern]
globbify = map compile
