module Main where

import           Hunt.CLI
import           Options.Applicative (execParser)


main :: IO ()
main = execParser huntCLI >>= runCmd
