module Hunt.CLI.Command.Server
  ( Command
  , run
  , parser
  ) where


import           Data.Monoid         ((<>))
import qualified Hunt.Server         as Server
import           Options.Applicative



-- SERVER


data Command
  = Start Server.HuntServerConfiguration
  deriving (Show)


-- PARSER


parser :: Parser Command
parser = Start <$> (Server.HuntServerConfiguration
  <$> hostOption
  <*> portOption
  <*> indexOption
  <*> logFileOption
  <*> logPriorityOption)
  where
    defaultHost     = Server.huntServerHost Server.defaultConfig
    defaultPort     = Server.huntServerPort Server.defaultConfig
    defaultLog      = Server.logFile Server.defaultConfig
    defaultDir      = Server.indexDirectory Server.defaultConfig
    defaultPriority = Server.logPriority Server.defaultConfig

    hostOption = strOption
      ( long "host"
      <> metavar "HOST"
      <> help ("Host of Hunt server, defaults to '" ++ defaultHost ++ "'"))
      <|> pure defaultHost

    portOption = option auto
      ( long "port"
      <> short 'p'
      <> metavar "PORT"
      <> help ("Port to run the server on, defaults to " ++ show defaultPort))
      <|> pure defaultPort

    indexOption = option auto
      ( long "index-directory"
      <> short 'i'
      <> metavar "DIR"
      <> help ("Directory for hunt to store data, defaults to \"$(pwd)/data\""))
      <|> pure defaultDir

    logFileOption = strOption
      ( long "logfile"
      <> metavar "LOGFILE"
      <> help ("File to write logs to, defaults to " ++ defaultLog ))
      <|> pure defaultLog

    logPriorityOption = option auto
      ( long "log-level"
      <> metavar "LEVEL"
      <> help ("Log level to use for logging, defaults to " ++ show defaultPriority ))
      <|> pure defaultPriority


-- RUN


run :: Command -> IO ()
run command =
  case command of
    Start config -> do
      let port = Server.huntServerPort config
      putStrLn $ "Starting server on port " ++ show port
      Server.runWithConfig Server.defaultConfig
