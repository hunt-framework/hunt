module Hunt.CLI.Command
  ( Command (..)
  , run
  , parser
  ) where


import           Data.Monoid             ((<>))
import qualified Hunt.CLI.Command.Client as Client
import qualified Hunt.CLI.Command.Server as Server
import           Options.Applicative



-- COMMAND


data Command
  = Client Client.Command
  | Server Server.Command
  deriving (Show)



-- PARSER


parser :: Parser Command
parser =
  subparser
  ( command "query" (info (Client <$> Client.parser <**> helper) (progDesc "Query a running hunt-server"))
  <> command "server" (info (Server <$> Server.parser <**> helper) (progDesc "Start a new Hunt server"))
  )



-- RUN


run :: Command -> IO ()
run command =
  case command of
    Client clientCommand ->
      Client.run clientCommand

    Server serverCommand ->
      Server.run serverCommand
