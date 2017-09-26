module Logger ( success, err, info, warning ) where

import System.Console.ANSI

-- | Print a message highlighting it
log' :: Color -> ColorIntensity -> String -> IO()
log' color intensity message = do
  setSGR [SetColor Foreground Dull White]
  -- TODO: print time here
  -- putStr "[" ++ minutes ++ ":" ++ seconds ++ "] "
  setSGR [Reset]
  setSGR [SetColor Foreground intensity color]
  print' message
  setSGR [Reset]

-- | Print a message in the console
print' :: String -> IO()
print' message = putStr $ unwords [message, "\n"]

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
