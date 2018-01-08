module Main
  ( main
  ) where


import qualified Hunt.CLI            as CLI
import           Options.Applicative (execParser)



-- MAIN


main :: IO ()
main =
  execParser CLI.parser >>= CLI.run
