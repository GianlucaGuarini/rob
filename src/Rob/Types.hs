{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, DeriveDataTypeable, OverloadedStrings #-}

module Rob.Types where

import GHC.Generics (Generic(..))
import qualified System.FilePath as FilePath
import Data.HashMap.Strict (HashMap)
import Data.Yaml
import Data.Maybe
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
data Template = Template String FilePath deriving (Generic, Show)

instance Eq Template where
  (Template nameA _) == (Template nameB _) = nameA == nameB

instance FromJSON Template
instance ToJSON Template

-- | Config file struct
newtype Config = Config {
  templates :: [Template]
} deriving (Generic, Show, Eq)

instance FromJSON Config
instance ToJSON Config

-- | Question struct
data Question =
    SimpleQuestion String String
  | PasswordQuestion String
  | ConfirmQuestion String Bool
  | SelectQuestion String [String] String
  | MultiselectQuestion String [String] [String]
  deriving (Show, Eq)

instance FromJSON Question where
  parseJSON (Object v) = do
    questionType <- v .:? "type" .!= "simple"
    parseJSON (Object v) >>= parseQuestion questionType

parseQuestion :: String -> Value -> Parser Question
parseQuestion questionType =
  case questionType of
    "confirm" -> withObject "ConfirmQuestion"
      (\o -> ConfirmQuestion <$> o .: questionKey <*> o .:? defaultKey .!= False)
    "select" -> withObject "SelectQuestion"
      (\o -> SelectQuestion <$> o .: questionKey <*> o .:? answersKey .!= [] <*> o .:? defaultKey .!= "")
    "multiselect" -> withObject "MultiselectQuestion"
      (\o -> MultiselectQuestion <$> o .: questionKey <*> o .:? answersKey .!= [] <*> o .:? defaultKey .!= [])
    "password" -> withObject "Password"
      (\o -> PasswordQuestion <$> o .: questionKey)
    _ -> withObject "SimpleQuestion"
      (\o -> SimpleQuestion <$> o .: questionKey <*> o .:? defaultKey .!= "")
  where
    questionKey = "question"
    answersKey = "answers"
    defaultKey = "default"

-- | Questionnaiere struct
data Questionnaire = Questionnaire {
  questions :: HashMap String Question,
  meta :: Object
} deriving (Generic, Show, Eq)

instance FromJSON Questionnaire

-- | Responses types
data Response =
    SingleResponse String
  | BooleanResponse Bool
  | MultipleResponse [String]
  deriving (Show, Eq)
