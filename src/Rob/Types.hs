{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Rob.Types where

import GHC.Generics (Generic(..))
import Data.HashMap.Strict (HashMap)
import Data.Yaml
import System.Console.CmdArgs

-- | Task struct listing all the available actions
data Task
  = Add {
    name :: String,
    path :: FilePath
  }
  | List
  | New deriving (Data, Typeable, Show)

-- | Template name + path
data Template = Template {
  name :: String,
  path :: FilePath
} deriving (Generic, Show, Ord, Eq)

instance FromJSON Template
instance ToJSON Template

-- | Config file struct
newtype Config = Config {
  templates :: [Template]
} deriving (Generic, Show, Ord, Eq)

instance FromJSON Config
instance ToJSON Config

-- | Answers types
data Answer = Value | AnswersList deriving (Generic, Show, Eq)
type Response = (String, Answer)
type AnswersList = [Answer]

instance FromJSON Answer
instance ToJSON Answer

-- | Data we will pass to the project templates
type TemplateData = HashMap Response

-- | Question struct
data QuestionMeta = QuestionMeta {
  question :: String,
  answers :: AnswersList,
  default' :: String,
  type' :: String
} deriving (Generic, Show, Eq)

instance FromJSON QuestionMeta where
    parseJSON (Object v) = do
        question <- v .: "question"
        answers  <- v .: "answers"
        type'    <- v .: "type"
        default' <- v .: "default"
        return QuestionMeta { question, answers, type', default' }

type Question = (String, QuestionMeta)
type QuestionsList = [Question]

-- | Questionnaiere struct
data Questionnaire = Questionnaire {
  questions :: QuestionsList,
  meta :: Object
} deriving (Generic, Show, Eq)

instance FromJSON Questionnaire
