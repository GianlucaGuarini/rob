module Rob.Tasks where

import qualified Rob.Package as Package
import Rob.UserMessages (newTaskHelp, addTaskHelp, listTaskHelp)
import Rob.Types (Tasks(..))

import System.Console.CmdArgs

list :: Task
list = List &= help listTaskHelp

-- | New task factory function
new :: Task
new = New &= help newTaskHelp

-- | Add task factory function
add :: Task
add = Add {
    Tasks.name = def &= typ "template-name" &= argPos 0,
    Tasks.path = def &= typ "path/to/the/template/folder" &= argPos 1
  } &= help addTaskHelp

-- | Export all the command line modes
mode :: Mode (CmdArgs Task)
mode = cmdArgsMode $ modes [new, add, list]
     &= help Package.description
     &= program Package.name
     &= summary (unwords [Package.name, "- v", Package.version, Package.author])
