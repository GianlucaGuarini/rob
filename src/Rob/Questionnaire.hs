{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rob.Questionnaire ( run, Questionnaire ) where
import Rob.Types (
  Questionnaire(..),
    TemplateData,
    QuestionMeta(..),
    QuestionsList,
    Answer,
    Question,
    Response
  )
import qualified Rob.Logger as Logger

import FortyTwo

import Data.HashMap.Strict (fromList)

-- | Get only the questions out of a questionnaier data struct
getQuestions :: Questionnaire -> QuestionsList
getQuestions Questionnaire { questions } = questions

-- | Run the questionnaire mapping all the answers to a TemplateData
run :: FilePath -> IO TemplateData
run path = do
  questionnaire <- decodeFile path
  responses <- mapM ask $ getQuestions $ fromJust questionnaire
  return $ fromList responses

ask :: Question -> IO Response
ask (key, QuestionMeta { question, answers, type', default' }) = do
  response <- handler
  return (key, response)
  where
    handler
      | type' == "input" = inputWithDefault question default'
      | type' == "confirm" = confirmWithDefault question default'
      | type' == "select" = selectWithDefault question answers default'
      | type' == "multiselect" = multiselectWithDefault question answers default'
      | type' == "password" = password question
      | otherwise = input question default'
