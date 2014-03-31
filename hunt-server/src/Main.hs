-- ----------------------------------------------------------------------------
{- |
  Main module for the executable.
-}
-- ----------------------------------------------------------------------------

module Main where

import           Data.Default

import           Hunt.Server            (start)
import           System.Console.CmdArgs (cmdArgs)

-- ------------------------------------------------------------

-- | Main function for the executable.
main :: IO ()
main = cmdArgs def >>= start

-- ------------------------------------------------------------
