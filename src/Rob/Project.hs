{-# LANGUAGE OverloadedStrings #-}

module Rob.Project where

import Rob.Types (Template(..), Blacklist)
import Rob.Logger (err, warning, success)
import Rob.UserMessages (parserError)

import Data.Yaml (Value)
import Control.Monad (forM_, unless)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BS
import Data.Text.Lazy.Encoding (encodeUtf8)
import System.FilePath.Posix (makeRelative)
import Data.List (isSuffixOf, isInfixOf, any, all, union, nub, intercalate)
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

ignoreFiles = [".gitignore", "svnignore.txt"]
knownIgnoredFiles = globbifyList [".git", ".svn", projectDataFile]

-- | Check if the path contains the questionnaire file
hasPathQuestionnaire :: FilePath -> IO Bool
hasPathQuestionnaire = doesFileExist . questionnaireFileByPath

-- | Get the questionnaire file by project path
questionnaireFileByPath :: FilePath -> FilePath
questionnaireFileByPath path = joinPath [path, projectDataFile]

-- | Create project files using the template and the user response received in the current directory
createFilesFromTemplate :: FilePath -> [(Text, Value)] -> IO()
createFilesFromTemplate projectRoot responses = pathWalkInterruptible projectRoot $ \tmpRoot dirs files -> do
    blacklist <- populateBlacklist blacklist tmpRoot
    parseDir projectRoot tmpRoot files blacklist responses
  where
    blacklist = []

-- | Add eventually new ignored files to the blacklist map
populateBlacklist :: Blacklist -> FilePath -> IO Blacklist
populateBlacklist blacklist root = do
    ignoredPatterns <- getIgnoredPatterns (map (\f -> joinPath [root, f]) ignoreFiles)
    return $ union blacklist [(root, ignoredPatterns ++ knownIgnoredFiles)]

-- | Parse a directory copying the files found into the current one
-- | where rob was called, it will also render eventually the answers to the questionnaire
-- | if template token will be found in any of the files
parseDir :: FilePath -> FilePath -> [FilePath] -> Blacklist -> [(Text, Value)] -> IO WalkStatus
parseDir root path files blacklist responses =
  if isInBlacklist path (takeFileName dirRelativePath) blacklist then
    return StopRecursing
  else do
    createDirectoryIfMissing True dirRelativePath
    forM_ files $ \file ->
      unless (isInBlacklist path file blacklist) $ do
        let fileAbsolutePath = joinPath [path, file]
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
    dirRelativePath = joinPath [makeRelative root path]
    templateData = fromPairs responses
    fallback e inPath outPath = do
      warning parserError
      putStrLn e
      file <- BS.readFile inPath
      BS.writeFile outPath file

-- | Check whether a path is blacklisted looking it up in the blacklist map
-- | here basically we try to emulate the gitignore behavior recursively
isInBlacklist :: FilePath -> FilePath -> Blacklist -> Bool
isInBlacklist root path = any $ \(f, p) -> all (\test -> test f p) [isChildDir, isInIgnoredList]
  where
    isChildDir folderPath _ = root `isInfixOf` folderPath
    isInIgnoredList _ = any (`match` path)

-- | Get all the files to ignore uniquelly from a list of known .ignore files
getIgnoredPatterns :: [FilePath] -> IO [Pattern]
getIgnoredPatterns files = do
  res <- mapM findIgnoredFilesList files
  return $ nub $ intercalate [] res

findIgnoredFilesList :: FilePath -> IO [Pattern]
findIgnoredFilesList f = do
  hasFile <- doesFileExist f
  if hasFile then do
    file <- readFile f
    return $ globbifyList $ cleanList $ lines file
  else return []
  where
    cleanList list = (\l -> (not . null) l && head l /= '#') `filter` list

-- | Map a list of file paths to glob patterns
globbifyList :: [FilePath] -> [Pattern]
globbifyList = map compile
