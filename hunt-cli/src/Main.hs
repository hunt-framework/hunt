module Main where

import           Hunt.CLI            (runCommand, huntCLI)
import           Options.Applicative (execParser)


main :: IO ()
main = execParser huntCLI >>= runCommand
