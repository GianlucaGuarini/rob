module Package (version, name, author) where

-- TODO: get these props from the .cabal file

import qualified Paths_rob
import Data.Version (showVersion)

-- | Return the Package version (stored in the .cabal file)
version :: String
version = showVersion Paths_rob.version

-- | Return the package name
name :: String
name = "rob"

-- | Return the package author
author :: String
author = "Author: Gianluca Guarini"
