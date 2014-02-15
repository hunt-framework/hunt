module Main where

import Paths_geoFrontend (getDataFileName)


import Hunt.GeoFrontend.Server (start)


main :: IO ()
main = do
    --p <- getDataFileName "map.xml"
    --everything <- readXML p
    
    --return ()
    start