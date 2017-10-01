{-# LANGUAGE DeriveDataTypeable #-}

module Tasks where

import qualified Package
import System.Console.CmdArgs

data Task
      = Add {
        name :: String,
        path :: FilePath
      }
      | New deriving (Data, Typeable, Show)

new :: Task
new = New &= help "Name of the template you want to use"

add :: Task
add = Add def def &= help "Add a new template"

mode :: Mode (CmdArgs Task)
mode = cmdArgsMode $ modes [new, add]
     &= help "Projects generator"
     &= program Package.name
     &= summary summary'

summary' :: String
summary' = unwords [Package.name, "- v", Package.version, Package.author]
