module Main where

import           Hunt.CLI
import           Options.Applicative


main :: IO ()
main = execParser opts >>= const (return ())
  where
    opts = info helper
      ( fullDesc
      <> progDesc "Query the server or work with a schema."
      <> header "A command line interface for the Hunt server."
      )
