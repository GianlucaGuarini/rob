{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Questionnaire ( run, Questionnaire ) where

import qualified GHC.Generics as GHC
import Data.Maybe
import Data.Yaml
import Data.Text (pack, Text)
import Data.HashMap.Strict (HashMap, fromList)
import Logger (info, warning, err)
import Text.Read (readMaybe)
import UserMessages (optionSelected, optionOutOfRange, invalidOptionValue, optionChoseOneOption)

-- | Aliases to simplify the multiple options handling
type Option = (Int, String)
type Options = [Option]

-- | Responses to the questionnaire
type ResponsesList = HashMap String String

-- | Answers types
type AnswersList = [String]

-- | Questionnaier struct
data Questionnaire = Questionnaire {
  questions :: [Question],
  meta :: Object
} deriving (GHC.Generic, Show, Eq)

instance FromJSON Questionnaire

-- | Question struct
data Question = Question {
  question :: String,
  answers :: AnswersList,
  default' :: String,
  type' :: String
} deriving (Show, Ord, Eq)

instance FromJSON Question where
    parseJSON (Object v) = do
        question <- v .: "question"
        answers  <- v .: "answers"
        type'    <- v .: "type"
        default' <- v .: "default"
        return Question { question, answers, type', default' }

-- | Simple question that requires a unique response
simpleQuestion :: Question -> IO String
simpleQuestion Question { question } = do print question; getLine

-- | Multiple responses handling
select :: Question -> AnswersList -> IO Int
select Question {question} list = do
  info question
  warning optionChoseOneOption
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
        info $ optionSelected $ getOptionLabel $ options !! normalizedOption
        return normalizedOption
      else do
        err $ optionOutOfRange options
        evaluateOption options
      where
        normalizedOption = option - 1
    Nothing -> do
      err invalidOptionValue
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
optionToString (optionId, label) = show optionId ++ ") " ++ label

-- | Get the option label
getOptionLabel :: Option -> String
getOptionLabel (_, label) = label

-- | Get only the questions out of a questionnaier data struct
getQuestions :: Questionnaire -> [Question]
getQuestions Questionnaire { questions } = questions

run :: FilePath -> IO ResponsesList
run path = do
  questionnaire <- decodeFile path
  return $ fromList $ map ask $ getQuestions $ fromJust questionnaire

ask :: Question -> (String, String)
ask Question { question, answers, type', default' }
  | type' == "bool" = ("title", "Bool")
  | type' == "list" = ("title", "List" )
  | type' == "string" = ("title", "String")
  | otherwise = ("title", "none")
