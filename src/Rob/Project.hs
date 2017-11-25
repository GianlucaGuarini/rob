{-# LANGUAGE OverloadedStrings #-}

module Rob.Project where

import Rob.Types (Template(..))
import Rob.Logger (warning, success)
import Rob.UserMessages (parserError, fileCreated)

import Data.Yaml (Value)
import Control.Monad (forM_, unless)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BS
import Data.Text.Lazy.Encoding (encodeUtf8)
import System.FilePath.Posix (makeRelative)
import Data.List (any, nub, intercalate)
import System.FilePath.Glob (match, simplify, compile, Pattern)
import Text.EDE (eitherRender, eitherParseFile, fromPairs)
import System.Directory.PathWalk (pathWalkInterruptible, WalkStatus(..))
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (joinPath, takeDirectory, normalise, isDrive, isValid)

-- | Get only the template name
getTemplateName :: Template -> String
getTemplateName (Template name _) = name

-- | Get only the template path
getTemplatePath :: Template -> FilePath
getTemplatePath (Template _ path) = path

-- | Get the template name by its path
getTemplatePathByName :: [Template] -> String -> FilePath
getTemplatePathByName [] [] = ""
getTemplatePathByName [] _ = ""
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

ignoreFiles :: [FilePath]
ignoreFiles = [".gitignore", "svnignore.txt"]

knownIgnoredFiles :: [Pattern]
knownIgnoredFiles = globbifyList [".git", ".svn", projectDataFile]

-- | Check if the path contains the questionnaire file
hasPathQuestionnaire :: FilePath -> IO Bool
hasPathQuestionnaire = doesFileExist . questionnaireFileByPath

-- | Get the questionnaire file by project path
questionnaireFileByPath :: FilePath -> FilePath
questionnaireFileByPath path = joinPath [path, projectDataFile]

-- | Start the template creation
createFilesFromTemplate :: FilePath -> [(Text, Value)] -> IO ()
createFilesFromTemplate root = walk knownIgnoredFiles root ""

-- | Walk recursively a folder copying its files using
walk :: [Pattern] -> FilePath -> FilePath -> [(Text, Value)] -> IO ()
walk currentBlacklist templateRoot currentPath responses =
  pathWalkInterruptible absolutePath $ \_ dirs files -> do
    blacklist <- getBlacklist
    render templateRoot relativePath files blacklist responses
    mapM_ (\f ->
        walk blacklist templateRoot (joinPath [relativePath, f]) responses
      )
      (whitelist dirs blacklist)
    return StopRecursing
    where
      absolutePath = joinPath [templateRoot, currentPath]
      relativePath = makeRelative templateRoot currentPath
      getBlacklist = do
        newBlacklist <- populateBlacklist absolutePath
        return $ currentBlacklist ++ newBlacklist
      whitelist dirs blacklist = [
          f | f <- dirs,
          not $ isInBlacklist f blacklist
        ]

-- | Add eventually new ignored files to the blacklist map
populateBlacklist :: FilePath -> IO [Pattern]
populateBlacklist root = getIgnoredPatterns (map (\f -> joinPath [root, f]) ignoreFiles)

-- | Parse a directory copying the files found into the current one
-- | where rob was called, it will also render eventually the answers to the questionnaire
-- | if template token will be found in any of the files
render :: FilePath -> FilePath -> [FilePath] -> [Pattern] -> [(Text, Value)] -> IO()
render templateRoot path files blacklist responses = do
  createDirectoryIfMissing True path
  forM_ files $ \file ->
    unless (isInBlacklist file blacklist) $ do
      let fileAbsolutePath = joinPath [templateRoot, path, file]
          fileRelativePath = joinPath [path, file]
      success $ fileCreated fileRelativePath
      template <- eitherParseFile fileAbsolutePath
      case template of
        Right t ->
          case eitherRender t templateData of
            Right res -> BS.writeFile fileRelativePath (encodeUtf8 res)
            Left e -> fallback e fileAbsolutePath fileRelativePath
        Left e -> fallback e fileAbsolutePath fileRelativePath
  where
    templateData = fromPairs responses
    fallback e inPath outPath = do
      warning parserError
      putStrLn e
      file <- BS.readFile inPath
      BS.writeFile outPath file

-- | Check whether a path is blacklisted looking it up in the blacklist map
-- | here basically we try to emulate the gitignore behavior recursively
isInBlacklist :: FilePath -> [Pattern] -> Bool
isInBlacklist path = any (`match` path)

-- | Get all the files to ignore uniquelly from a list of known .ignore files
getIgnoredPatterns :: [FilePath] -> IO [Pattern]
getIgnoredPatterns files = do
  res <- mapM findIgnoredFilesList files
  return $ nub $ intercalate [] res

-- | Figure out which files must be ignored reading them from the .gitignore
findIgnoredFilesList :: FilePath -> IO [Pattern]
findIgnoredFilesList f = do
  hasFile <- doesFileExist f
  if hasFile then do
    file <- readFile f
    return $ (
        globbifyList .
        extendIgnoredFiles .
        cleanList
      ) $ lines file
  else return []
  where
    cleanList list = (\l -> (not . null) l && head l /= '#') `filter` list

-- | Extend the ignored files in order to enhance the patterns matching
-- | for example with the "/node_modules/*" pattern we will add also "/node_modules"
-- | to the excluded folders
extendIgnoredFiles :: [FilePath] -> [FilePath]
extendIgnoredFiles (x:xs) =
    if (not . null) extension then
      x : extension : extendIgnoredFiles xs
    else x : extendIgnoredFiles xs
  where
    extension = if all (\t -> t dirName) tests then dirName else []
    dirName   = (takeDirectory . normalise) x
    tests = [
        isValid,
        not . isDrive,
        not . isDot,
        not . isWildCard,
        not . isDoubleWildCard
      ]
extendIgnoredFiles [] = []

-- | Helpers to enhance the ignored files
isDot :: FilePath -> Bool
isDot = (==) "."

isWildCard :: FilePath -> Bool
isWildCard = (==) "*"

isDoubleWildCard :: FilePath -> Bool
isDoubleWildCard = (==) "**"

-- | Map a list of file paths to glob patterns
globbifyList :: [FilePath] -> [Pattern]
globbifyList = map (simplify . compile)
