{-# LANGUAGE NamedFieldPuns #-}
module Rob.Questionnaire ( run, Questionnaire ) where
import Rob.Types
import qualified Rob.Logger as Logger
import FortyTwo
import Data.Yaml
import Data.Maybe
import qualified System.FilePath as FilePath
import Data.HashMap.Strict (fromList, toList)

-- | Get only the questions out of a questionnaier data struct as list
getQuestions :: Questionnaire -> [(String, Question)]
getQuestions Questionnaire { questions } = toList questions

-- | Run the questionnaire
run :: FilePath -> IO [(String, Response)]
run path = do
  questionnaire <- decodeFile path
  mapM mapQuestion (getQuestions (fromJust questionnaire))

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
