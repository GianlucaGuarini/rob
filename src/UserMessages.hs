module UserMessages where

import qualified Package

-- Questions messages

choseATemplate :: String
choseATemplate = "Which template do you want to use?"

optionSelected :: String -> String
optionSelected option = "Congrats! You have chosen the option: " ++ option

optionChoseOneOption :: String
optionChoseOneOption = unlines ["Chose between one of these options:"]

optionOutOfRange :: [a] -> String
optionOutOfRange list = unwords [
    "The option selected is not whithin the range of options provided",
    "(1.." ++ show (length list) ++ ")"
  ]

invalidOptionValue :: String
invalidOptionValue = unwords [
    "Are you sure you have inserted a valid number?",
    "Please try again."
  ]

-- Config messages
configFileFound :: FilePath -> String
configFileFound path = unwords [path, "was found!"]

noConfigFileFound :: String -> String
noConfigFileFound configFileName = unwords [
    "No",
    configFileName,
    "file was found in your $HOME path"
  ]

configFileCreated :: FilePath -> [String]
configFileCreated path = [
    "Creating a new config file in:",
    path
  ]

-- Tasks help
newTaskHelp :: String
newTaskHelp = "Create a new project in the current folder"

addTaskHelp :: String
addTaskHelp = "Add a new project template"

-- Errors
noTemplatesAvailable :: String
noTemplatesAvailable = "It was not possible to find any project template. Have you ever tried adding one?"

tryAddingATemplate :: String
tryAddingATemplate = "Try: " ++ Package.name ++ " " ++ "new template-name path/to/the/template/folder"

projectPathDoesNotExist :: FilePath -> String
projectPathDoesNotExist path = unwords [
    "The path to",
    "\"" ++ path ++ "\"",
    "does not exist!"
  ]

projectQuestionnaireMissing :: FilePath -> String
projectQuestionnaireMissing path = unwords [
    "It was not possible to find any project.yml in",
    "\"" ++ path ++ "\"",
    "Please make sure to have a questionnaire project.yml in your template!"
  ]

projectAdded :: String -> String
projectAdded name = unwords [
    name,
    "was added to your projects templates!"
  ]
