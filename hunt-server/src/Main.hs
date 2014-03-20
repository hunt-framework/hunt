module Main where

import           Hunt.Server            (start)
import           Hunt.Server.Common
import           System.Log.Logger      (Priority(..))
import           System.Console.CmdArgs (cmdArgs, explicit, help, name, program,
                                         summary, typ, typFile, (&=))

-- ----------------------------------------------------------------------------

huntConfiguration :: HuntServerConfiguration
huntConfiguration = HuntServerConfiguration {
    huntServerHost = "*"
      &= explicit &= name "host"
      &= help "Bind to altenate host (* = any, *4 = any IPv4, *6 = any IPv6)",
    huntServerPort = (3000::Int)
      &= explicit &= name "port"
      &= help "Listen on alternate port",
    readIndexOnStartup = Nothing
      &= explicit &= name "load-index" &= typFile
      &= help "Load index on startup",
    logPriority = DEBUG
      &= explicit &= name "log-priority" &= typ "PRIORITY"
      &= help "Set log level for stdout",
    logFile = "hunt.log"
      &= explicit &= name "log-file" &= typFile
      &= help "Set logfile location"
  } &= summary "Standalone search server based on the hunt searchengine."
    &= program "hunt-server"

-- ----------------------------------------------------------------------------

main :: IO ()
main = do
  config <- cmdArgs huntConfiguration
  start config
