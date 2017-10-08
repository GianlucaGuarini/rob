module Questions ( ask, select, Option ) where

import Logger (info, warning, err)
import Text.Read (readMaybe)
import UserMessages (optionSelected, optionOutOfRange, invalidOptionValue, optionChoseOneOption)

-- | Aliases to simplify the multiple options handling
type Option = (Int, String)
type Options = [Option]

-- | Simple question that requires a unique response
ask :: String -> IO String
ask question = do print question; getLine

-- | Multiple responses handling
select :: String -> [String] -> IO Int
select question list = do
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
createOptions :: [String] -> Options
createOptions list = [(i, list !! (i - 1)) | i <- [1..(length list)]]

-- | Convert an option to string
optionToString :: Option -> String
optionToString (optionId, label) = show optionId ++ ") " ++ label

-- | Get the option label
getOptionLabel :: Option -> String
getOptionLabel (_, label) = label
