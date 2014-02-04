module Main where

import Hunt.Server (start)
import Hunt.Server.Common
import System.Console.CmdArgs (cmdArgs, (&=), explicit, name, help, summary, typFile)

hayooConfiguration :: HuntServerConfiguration
hayooConfiguration = HuntServerConfiguration {
        huntServerHost = "*"  &= explicit &= name "host" 
            &= help "Which host to bind: * means any, *4 means any IPv4, *6 means any IPv6, you can also specify a specific host.",
        huntServerPort = (3000::Int)  &= explicit &= name "port" 
            &= help "Listen on this Port",
        readIndexOnStartup = Nothing &= explicit &= name "load-index" &= typFile
            &= help "If given, this index will be loaded on startup"
    } &= summary "Standalone search server based on hunt searchengine"

main :: IO ()
main = do
    config <- cmdArgs hayooConfiguration
    start config
    
