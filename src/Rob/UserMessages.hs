module Rob.UserMessages where

import qualified Rob.Package as Package

-- Questions messages

choseATemplate :: String
choseATemplate = "Which template do you want to use?"

choseATemplateToDelete :: String
choseATemplateToDelete = "Which template do you want to delete?"

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

listTaskHelp :: String
listTaskHelp = "List all the available projects templates"

removeTaskHelp :: String
removeTaskHelp = "Remove a template from the list of the ones available"

-- Errors

noTemplatesAvailable :: String
noTemplatesAvailable = "It was not possible to find any project template. Have you ever tried adding one?"

noTemplateSelected :: String
noTemplateSelected = "No template selected. Please select one the list"

tryAddingATemplate :: String
tryAddingATemplate = "Try: " ++ Package.name ++ " " ++ "new template-name path/to/the/template/folder"

unableToParseQuestionnaire :: String
unableToParseQuestionnaire = "It was not able to parse the project questionnaire"


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

parserError :: String
parserError = "There was a parser error, this file will be only copied"

-- General info

availableTemplates :: String
availableTemplates = "Available Templates:"

-- Success messages

projectSuccessfullyCreated :: String
projectSuccessfullyCreated = "Your project was successfully created"

-- Helpers

emptyString :: String
emptyString = ""
