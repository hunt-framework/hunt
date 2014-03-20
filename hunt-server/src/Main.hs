module Main where

import           Data.Default

import           Hunt.Server            (start)
import           System.Console.CmdArgs (cmdArgs)

-- ----------------------------------------------------------------------------

main :: IO ()
main = cmdArgs def >>= start
