module Main where

import           Hunt.Server            (start)
import           Hunt.Server.Common
import           System.Console.CmdArgs (cmdArgs, explicit, help, name, program,
                                         summary, typFile, (&=))

-- ----------------------------------------------------------------------------

huntConfiguration :: HuntServerConfiguration
huntConfiguration = HuntServerConfiguration {
    huntServerHost = "*"  &= explicit &= name "host"
      &= help "Bind to altenate host (* = any, *4 = any IPv4, *6 = any IPv6)",
    huntServerPort = (3000::Int)  &= explicit &= name "port"
      &= help "Listen on alternate port",
    readIndexOnStartup = Nothing &= explicit &= name "load-index" &= typFile
      &= help "Load index on startup"
  } &= summary "Standalone search server based on the hunt searchengine."
    &= program "hunt-server"

-- ----------------------------------------------------------------------------

main :: IO ()
main = do
  config <- cmdArgs huntConfiguration
  start config
