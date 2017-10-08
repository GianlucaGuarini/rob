module Questions ( ask, select, Option ) where

import Logger (info, warning, err)
import UserMessages (optionSelected, optionOutOfRange, invalidOptionValue, optionChoseOneOption)

type Option = (Int, String)
type Options = [Option]

ask :: String -> IO String
ask question = do print question; getLine

select :: String -> [String] -> IO Int
select question list = do info question
                          warning optionChoseOneOption
                          let options = createOptions list
                          putStrLn $ unlines $ map optionToString options
                          evaluateOption options

evaluateOption :: Options -> IO Int
evaluateOption options = do answer <- getLine
                            let maybeOption = readMaybe answer :: Maybe Int
                            case maybeOption of
                             Just option -> if isValidOption options option then do
                                              let normalizedOption = option - 1
                                              info $ optionSelected $ getOptionLabel $ options !! normalizedOption
                                              return normalizedOption
                                            else do
                                              err $ optionOutOfRange options
                                              evaluateOption options
                             Nothing -> do err invalidOptionValue
                                           evaluateOption options


readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing


isValidOption :: Options -> Int -> Bool
isValidOption options answer =  answer `elem` [1..(length options)]

createOptions :: [String] -> Options
createOptions list = [(i, list !! (i - 1)) | i <- [1..(length list)]]

optionToString :: Option -> String
optionToString (optionId, label) = show optionId ++ ") " ++ label

getOptionLabel :: Option -> String
getOptionLabel (_, label) = label
