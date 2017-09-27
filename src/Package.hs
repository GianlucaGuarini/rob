module Package (version) where

import qualified Paths_rob
import Data.Version (showVersion)

-- | Return the Package version (stored in the .cabal file)
version :: String
version = showVersion Paths_rob.version
