module Logger ( success, err, info, warning, flatten ) where

import System.Console.ANSI
import Data.Time.Clock
import Data.Time.Format

-- | Print a message highlighting it
log' :: Color -> ColorIntensity -> String -> IO()
log' color intensity message = do
  setSGR [SetColor Foreground Dull White]
  time <- getCurrentTime
  putStr $ formatTime' time
  setSGR [Reset]
  setSGR [SetColor Foreground intensity color]
  print' $ message ++ "\n"
  setSGR [Reset]

-- | Format the time, showing it into the log message
formatTime' :: UTCTime -> String
formatTime' = formatTime defaultTimeLocale "[%T] "

-- | Print a message in the console
print' :: String -> IO()
print' = putStr

-- | Log success messages
success :: String -> IO()
success = log' Green Dull

-- | Log error messages
err :: String -> IO()
err = log' Red Vivid

-- | Log info messages
info :: String -> IO()
info = log' Cyan Dull

-- | Log warning messages
warning :: String -> IO()
warning = log' Yellow Dull

-- | Log a raw message without using colors
raw :: String -> IO()
raw = print'

-- | Execute log on a list of messages
flatten :: Monad m => (a -> m b) -> [a] -> m ()
flatten = mapM_
