{-# LANGUAGE DeriveDataTypeable #-}

module Tasks where

import qualified Package
import System.Console.CmdArgs

-- | Task struct listing all the available actions
data Task
  = Add {
    name :: String,
    path :: FilePath
  }
  | New deriving (Data, Typeable, Show)

-- | New task factory function
new :: Task
new = New &= help "Name of the template you want to use"

-- | Add task factory function
add :: Task
add = Add def def &= help "Add a new template"

-- | Export all the command line modes
mode :: Mode (CmdArgs Task)
mode = cmdArgsMode $ modes [new, add]
     &= help Package.description
     &= program Package.name
     &= summary (unwords [Package.name, "- v", Package.version, Package.author])
