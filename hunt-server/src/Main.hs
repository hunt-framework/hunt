module Main where

import           Data.Monoid         ((<>))
import           Hunt.Server         (HuntServerConfiguration (..),
                                      defaultConfig, runWithConfig)
import           Options.Applicative

-- MAIN

main :: IO ()
main = execParser opts >>= runHuntServer
  where
    opts = info (helper <*> configuration)
      ( fullDesc
      <> progDesc "Start the Hunt server"
      <> header "Hunt server" )


-- | Run the server
runHuntServer :: HuntServerConfiguration -> IO ()
runHuntServer config = do
  let port = huntServerPort config
  putStrLn $ "Starting server on port " ++ show port
  runWithConfig defaultConfig


-- ARGUMENTS

configuration :: Parser HuntServerConfiguration
configuration = HuntServerConfiguration
  <$> hostOption
  <*> portOption
  <*> indexOption
  <*> logFileOption
  <*> logPriorityOption
  where
    defaultHost     = huntServerHost defaultConfig
    defaultPort     = huntServerPort defaultConfig
    defaultLog      = logFile defaultConfig
    defaultDir      = indexDirectory defaultConfig
    defaultPriority = logPriority defaultConfig

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
