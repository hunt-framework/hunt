module Main where

import Holumbus.GeoFrondend.Feeder
import Paths_geoFrontend (getDataFileName)

main :: IO ()
main = do
    p <- getDataFileName "map.xml"
    readXML p
    return ()