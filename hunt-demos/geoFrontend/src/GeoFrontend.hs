module Main where

import Paths_geoFrontend (getDataFileName)


import Hunt.GeoFrontend.Server (start)
import Hunt.GeoFrontend.Common (GeoFrontendConfiguration (..))

import System.Console.CmdArgs (cmdArgs, (&=), explicit, name, help, summary, typFile)

geoFrontendConfiguration :: GeoFrontendConfiguration
geoFrontendConfiguration = GeoFrontendConfiguration {
        geoFrontendHost = "*"  &= explicit &= name "geo-demo-host"
            &= help "Which host to bind: * means any, *4 means any IPv4, *6 means any IPv6, you can also specify a specific host.",
        geoFrontendPort = (8080::Int)  &= explicit &= name "geo-demo-port"
            &= help "Listen on this Port",
        huntUrl = "http://localhost:3000/" &= explicit &= name "hunt-url"
            &= help "Url of the Hunt Search Engine (default: http://localhost:3000/)",
        loadIndex = Nothing &= explicit &= name "load-index" &= typFile
            &= help "Load index on startup"
    } &= summary "GeoFrontend Search Frontend"

main :: IO ()
main = do
    config <- cmdArgs geoFrontendConfiguration
    start config
