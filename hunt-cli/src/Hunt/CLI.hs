module Hunt.CLI
  ( P.huntCLI
  , runCmd
  ) where

import Hunt.CLI.Types
import qualified Hunt.CLI.Parser as P


-- EXECUTING COMMANDS

runCmd :: Command -> IO ()
runCmd = putStrLn . show
