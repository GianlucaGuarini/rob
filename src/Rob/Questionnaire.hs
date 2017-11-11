{-# LANGUAGE NamedFieldPuns #-}
module Rob.Questionnaire where
import Rob.Types
import Rob.Project (questionnaireFileByPath, hasPathQuestionnaire)
import Rob.UserMessages (unableToParseQuestionnaire, projectQuestionnaireMissing)
import qualified Rob.Logger as Logger

import System.Exit (exitFailure)
import FortyTwo
import Data.Yaml
import qualified Data.Vector as V
import Data.Text (Text, pack)
import Data.HashMap.Strict (HashMap, toList)

-- | Get only the questions out of a questionnaier data struct as list
getQuestions :: Questionnaire -> [(Text, Question)]
getQuestions Questionnaire { questions } = toList questions

-- | Run the questionnaire
run :: FilePath -> IO [(Text, Value)]
run path = do
  hasQuestionnaireFile <- hasPathQuestionnaire path
  if hasQuestionnaireFile then do
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
mapQuestion :: (Text, Question) -> IO (Text, Value)
mapQuestion (key, q) = do
  answer <- ask q
  return (key, answer)

-- Ask a Question to get a response from the user
ask :: Question -> IO Value
ask (PasswordQuestion question) = do
  res <- password question
  return (String $ pack res)
ask (SimpleQuestion question defaultValue) = do
  res <- inputWithDefault question defaultValue
  return (String $ pack res)
ask (SelectQuestion question answers defaultValue) = do
  res <- selectWithDefault question answers defaultValue
  return (String $ pack res)
ask (ConfirmQuestion question defaultValue) = do
  res <- confirmWithDefault question defaultValue
  return (Bool res)
ask (MultiselectQuestion question answers defaultValues) = do
  res <- multiselectWithDefault question answers defaultValues
  return (Array $ V.fromList $ map (String . pack) res)
