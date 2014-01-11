-- {-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Holumbus.GeoFrondend.Feeder where

import GHC.Generics (Generic)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Lens (over, mapped, both)

import Data.Maybe (isJust, fromJust)
import Data.Text (Text, pack)
import qualified Data.ByteString.Char8 as B (unpack)
import qualified Data.ByteString.Lazy as BL

import Data.Aeson (FromJSON, ToJSON, decode, encode, Object, object, toJSON, (.=))

import Data.Geo.OSM
import Data.Geo.OSM.NWRCommon


{-}
import Control.Lens

-- Some nice lenses to go with it
makeLenses ''OSM

makePrisms ''OSM
-}

writeJSON :: MonadIO m => FilePath -> m ()
writeJSON p = do
    nwrs <- readXML p
    let docs = map nwrToGeoDocument nwrs
    liftIO $ writeFile "map.json" (B.unpack $ BL.toStrict $ encode docs)



readXML :: MonadIO m => FilePath -> m [NodeWayRelation]
readXML p = do
    [osm] <- liftIO $ Data.Geo.OSM.readOsmFile p
    let nwrs = chGetNWR $ osmChildren osm
    let namedNwrs = filter nwrHasName nwrs
    return $ filter (\n -> (isJust $ asNode n) || (isJust $ asWay n)) namedNwrs

nwrHasName :: NodeWayRelation -> Bool
nwrHasName nwr = not . null $ filter (("name" == ) . tagGetKey) $ nwrCommonGetTags $ getCommon nwr 

getCommon :: NodeWayRelation -> NWRCommon
getCommon nwr
    | isJust $ asNode nwr = nodeGetCommon $ fromJust $ asNode nwr
    | isJust $ asWay nwr = wayGetCommon $ fromJust $ asWay nwr
    | isJust $ asRelation nwr = relGetCommon $ fromJust $ asRelation nwr
    | otherwise = error "getCommon: Please Report a bug"


data GeoDocument = GeoDocument {
    name :: Text,
    lon  :: Text,
    lat  :: Text,
    kind :: OSMType,
    tags :: [(Text, Text)]
} deriving (Eq)

instance ToJSON GeoDocument where
    toJSON d = object $ [ "name" .= name d,
                        "lon" .= lon d,
                        "lat" .= lat d,
                        "kind" .= kind d
                        ] ++ (map (uncurry (.=)) $ tags d)

data OSMType = Way | Node
    deriving (Show, Eq, Generic)

instance ToJSON OSMType

nwrToGeoDocument :: NodeWayRelation -> GeoDocument
nwrToGeoDocument nwr = GeoDocument nameTag nwrLon nwrLat kind' (filter (\(k,_) -> k /= "name") tags')
    where
        tags' :: [(Text,Text)]
        tags' = over (mapped.both) pack $ fmap tagToPair $ nwrCommonGetTags $ getCommon nwr
        nameTag :: Text
        nameTag = fromJust $ lookup "name" tags'
        (nwrLon, nwrLat) = over both pack $ nwrGetCoords nwr
        kind' = foldNodeWayRelation nwr (const Node) (const Way) undefined

nwrGetCoords :: NodeWayRelation -> (String, String)
nwrGetCoords nwr = foldNodeWayRelation nwr fNode fWay undefined
    where
        fNode n = (nodeGetLon n, nodeGetLat n)
        fWay _ = ("", "")