{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Questionnaire ( run, Questionnaire ) where

import qualified GHC.Generics as GHC
import Data.Maybe
import Data.Yaml
import System.Console.Questioner (prompt)
import Data.Text (pack, Text)
import Data.HashMap.Strict (HashMap, fromList)
import qualified Logger
import Text.Read (readMaybe)
import UserMessages (optionSelected, optionOutOfRange, invalidOptionValue, optionChoseOneOption)

-- | Aliases to simplify the multiple options handling
type Option = (Int, Answer)
type Options = [Option]

-- | Answers types
data Answer =
    Bool' Bool
  | String' String
  | List' AnswersList
  deriving (GHC.Generic, Show, Ord, Eq)
type AnswersList = [Answer]

-- | Responses to the questionnaire
type ResponsesList = HashMap String Answer

instance FromJSON Answer
instance ToJSON Answer

-- | Questionnaier struct
type KeyAndQuestion = (String, Question)
data Questionnaire = Questionnaire {
  questions :: [KeyAndQuestion],
  meta :: Object
} deriving (GHC.Generic, Show, Eq)

instance FromJSON Questionnaire

-- | Question struct
data Question = Question {
  question :: String,
  answers :: AnswersList,
  default' :: String,
  type' :: String
} deriving (GHC.Generic, Show, Ord, Eq)

instance FromJSON Question where
    parseJSON (Object v) = do
        question <- v .: "question"
        answers  <- v .: "answers"
        type'    <- v .: "type"
        default' <- v .: "default"
        return Question { question, answers, type', default' }



-- | Multiple responses handling
select :: Question -> AnswersList -> IO Int
select Question {question} list = do
  Logger.ask question
  Logger.warning optionChoseOneOption
  putStrLn $ unlines $ map optionToString options
  evaluateOption options
  where
    options = createOptions list

-- | Check if the option provided by the user is valid
evaluateOption :: Options -> IO Int
evaluateOption options = do
  answer <- getLine
  case maybeOption answer of
    Just option ->
      if isOptionInRange options option
      then do
        Logger.info $ optionSelected $ getOptionLabel $ options !! normalizedOption
        return normalizedOption
      else do
        Logger.err $ optionOutOfRange options
        evaluateOption options
      where
        normalizedOption = option - 1
    Nothing -> do
      Logger.err invalidOptionValue
      evaluateOption options
    where
      -- hoping to get an int here
      maybeOption a = readMaybe a :: Maybe Int

-- | Check if the option provided is available in the current options list
isOptionInRange :: Options -> Int -> Bool
isOptionInRange options answer =  answer `elem` [1..(length options)]

-- | Create the options list by a list of strings
createOptions :: AnswersList -> Options
createOptions list = [(i, list !! (i - 1)) | i <- [1..(length list)]]

-- | Convert an option to string
optionToString :: Option -> String
optionToString (optionId, label) = show optionId ++ ") " ++ show label

-- | Get the option label
getOptionLabel :: Option -> String
getOptionLabel (_, label) = show label

-- | Get only the questions out of a questionnaier data struct
getQuestions :: Questionnaire -> [KeyAndQuestion]
getQuestions Questionnaire { questions } = questions

-- | Print the default value for any answer
-- | If it's null we skip it
printDefault :: String -> IO()
printDefault s =
  if null s then
    return ()
  else
    Logger.warning $ "Default Value:" ++ s

-- | Simple question that requires a unique response
input :: IO String
input = do
  res <- getLine
  Logger.info res
  return res

-- | Boolean question either yes or no
confirm :: IO Bool
confirm = do
  Logger.warning "(y/N)"
  res <- getLine
  Logger.info res
  case res of
    "y"   -> return True
    "yes" -> return True
    _     -> return False

-- | Run the questionnaire mapping all the answers to a ResponsesList
run :: FilePath -> IO ResponsesList
run path = do
  questionnaire <- decodeFile path
  responses <- mapM ask $ getQuestions $ fromJust questionnaire
  return $ fromList responses

ask :: KeyAndQuestion -> IO (String, Answer)
ask (key, Question { question, answers, type', default' }) = do
  Logger.ask question;
  printDefault default'
  case type' of
    "input"         -> input >>= \answer -> return (key, String' answer)
    "confirm"       -> confirm >>= \answer -> return (key, Bool' answer)
    "select"        -> return ("title", List' [Bool' True, Bool' False] )
    "multiselect"   -> return ("title", List' [Bool' True, Bool' False] )
    _               -> return ("title", Bool' False)
