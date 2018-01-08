module Hunt.CLI
  ( CLI.run
  , parser
  ) where


import           Data.Monoid         ((<>))
import qualified Hunt.CLI.Command    as CLI
import qualified Hunt.Client         as HC
import           Options.Applicative



-- PARSER


-- | Build the full parser for parsing a @Command@
-- from command line arguments.
parser :: ParserInfo CLI.Command
parser  = info (helper <*> CLI.parser)
    (  fullDesc
    <> progDesc "Run the following command"
    <> header "A central command line interface for interacting with Hunt."
    )
