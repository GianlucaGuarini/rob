module Main ( main ) where

import qualified Config
-- import qualified Questions

main :: IO ()
main = do
  config <- Config.get
  print config
