module Hunt.CLI
  ( P.huntCLI
  , runCmd
  ) where

import qualified Hunt.CLI.Parser as P
import           Hunt.CLI.Types


-- EXECUTING COMMANDS

runCmd :: Command -> IO ()
runCmd = putStrLn . show
