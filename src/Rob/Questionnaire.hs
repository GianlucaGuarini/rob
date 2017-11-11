{-# LANGUAGE NamedFieldPuns #-}
module Rob.Questionnaire where
import Rob.Types
import Rob.UserMessages (unableToParseQuestionnaire, projectQuestionnaireMissing)
import qualified Rob.Logger as Logger

import System.Exit (exitFailure)
import FortyTwo
import Data.Yaml
import Data.Maybe
import System.Directory (doesFileExist)
import System.FilePath.Posix (joinPath)
import Data.HashMap.Strict (fromList, toList)

-- | Check if the path contains the questionnaire file
hasPathQuestionnaire :: FilePath -> IO Bool
hasPathQuestionnaire = doesFileExist . questionnaireFileByPath

-- | Get the questionnaire file by project path
questionnaireFileByPath :: FilePath -> FilePath
questionnaireFileByPath path = joinPath [path, "project.yml"]

-- | Get only the questions out of a questionnaier data struct as list
getQuestions :: Questionnaire -> [(String, Question)]
getQuestions Questionnaire { questions } = toList questions

-- | Run the questionnaire
run :: FilePath -> IO [(String, Response)]
run path = do
  hasQuestionnaireFile <- hasPathQuestionnaire path
  if hasQuestionnaireFile then do
    -- print $ show questionnaireFile
    questionnaire <- decodeFileEither questionnaireFile
    case questionnaire of
      Right q -> mapM mapQuestion (getQuestions q)
      Left e -> do
        Logger.err unableToParseQuestionnaire
        Logger.warning $ show e
        exitFailure
  else do
    Logger.err $ projectQuestionnaireMissing path
    exitFailure

  where
    questionnaireFile = questionnaireFileByPath path


-- Map all the questions to answers
mapQuestion :: (String, Question) -> IO (String, Response)
mapQuestion (key, q) = do
  answer <- ask q
  return (key, answer)

-- Ask a Question to get a response from the user
ask :: Question -> IO Response
ask (PasswordQuestion question) = do
  res <- password question
  return (SingleResponse res)
ask (SimpleQuestion question defaultValue) = do
  res <- inputWithDefault question defaultValue
  return (SingleResponse res)
ask (SelectQuestion question answers defaultValue) = do
  res <- selectWithDefault question answers defaultValue
  return (SingleResponse res)
ask (ConfirmQuestion question defaultValue) = do
  res <- confirmWithDefault question defaultValue
  return (BooleanResponse res)
ask (MultiselectQuestion question answers defaultValues) = do
  res <- multiselectWithDefault question answers defaultValues
  return (MultipleResponse res)
