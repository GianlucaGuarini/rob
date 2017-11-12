module Rob.Tasks where

import qualified Rob.Package as Package
import Rob.UserMessages (newTaskHelp, addTaskHelp, listTaskHelp, removeTaskHelp)
import qualified Rob.Types as Types (Task(..))

import System.Console.CmdArgs

list :: Types.Task
list = Types.List &= help listTaskHelp

-- | New task factory function
new :: Types.Task
new = Types.New &= help newTaskHelp

-- | New task factory function
remove :: Types.Task
remove = Types.Remove &= help removeTaskHelp

-- | Add task factory function
add :: Types.Task
add = Types.Add {
    Types.name = def &= typ "template-name" &= argPos 0,
    Types.path = def &= typ "path/to/the/template/folder" &= argPos 1
  } &= help addTaskHelp

-- | Export all the command line modes
mode :: Mode (CmdArgs Types.Task)
mode = cmdArgsMode $ modes [new, add, remove, list]
     &= help Package.description
     &= program Package.name
     &= summary (unwords [Package.name, "- v", Package.version, Package.author])
