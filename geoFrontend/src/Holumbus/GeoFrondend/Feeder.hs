-- {-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Holumbus.GeoFrondend.Feeder where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Lens (over, mapped, both)

import Data.Maybe (isJust, fromJust)
import Data.List (intersect)
import qualified Data.Set as Set

import Data.Text (Text, pack)
import qualified Data.Text as T

import qualified Data.ByteString.Char8 as B (unpack)
import qualified Data.ByteString.Lazy as BL

--import Data.Aeson (FromJSON, ToJSON, decode, encode, Object, object, toJSON, (.=))
import qualified Data.Aeson as JSON

import Data.Geo.OSM
import Data.Geo.OSM.NWRCommon

import Holumbus.GeoFrondend.Common




writeJSON :: MonadIO m => FilePath -> m ()
writeJSON p = do
    nwrs <- readXML p
    let docs = Set.fromList $ map nwrToGeoDocument nwrs
    liftIO $ writeFile "map.json" (B.unpack $ BL.toStrict $ JSON.encode docs)



readXML :: MonadIO m => FilePath -> m [NodeWayRelation]
readXML p = do
    [osm] <- liftIO $ Data.Geo.OSM.readOsmFile p
    let nwrs = chGetNWR $ osmChildren osm
    let namedNwrs = filter nwrHasName nwrs
    return $ filter (\n -> (isJust $ asNode n) || (isJust $ asWay n)) namedNwrs

supportedTags :: [Tag] -> Bool
supportedTags tags' =  not . null $ ["amenity", "highway", "historic", "shop", "tourism", "leisure", "website", "wikipedia"] `intersect` (fst $ unzip $ map tagToPair tags')

hasNameTag :: [Tag] -> Bool
hasNameTag tags' = not . null $ filter (("name" == ) . tagGetKey) tags'

noStopWords :: [Tag] -> Bool
noStopWords tags' = not $ or $ map (\x -> x `elem` tags') [tag "amenity" "toilets", tag "highway" "bus_stop"]


nwrHasName :: NodeWayRelation -> Bool
nwrHasName nwr =  hasNameTag tags' && supportedTags tags' && noStopWords tags'
    where
        tags' = nwrCommonGetTags $ getCommon nwr 

getCommon :: NodeWayRelation -> NWRCommon
getCommon nwr
    | isJust $ asNode nwr = nodeGetCommon $ fromJust $ asNode nwr
    | isJust $ asWay nwr = wayGetCommon $ fromJust $ asWay nwr
    | isJust $ asRelation nwr = relGetCommon $ fromJust $ asRelation nwr
    | otherwise = error "getCommon: Please Report a bug"


nwrToGeoDocument :: NodeWayRelation -> GeoDocument
nwrToGeoDocument nwr = GeoDocument osmId' nameTag nwrLon nwrLat kind' (filter (\(k,_) -> k /= "name") tags')
    where
        osmId' = read $ nwrCommonGetId $ getCommon nwr
        tags' :: [(Text,Text)]
        tags' = over (mapped.both) pack $ filterIndexedTags $ fmap tagToPair $ nwrCommonGetTags $ getCommon nwr
        nameTag :: Text
        nameTag = fromJust $ lookup "name" tags'
        (nwrLon, nwrLat) = nwrGetCoords nwr
        kind' = foldNodeWayRelation nwr (const Node) (const Way) undefined

nwrGetCoords :: NodeWayRelation -> (Double, Double)
nwrGetCoords nwr = foldNodeWayRelation nwr fNode fWay undefined
    where
        fNode n = over both read $ (nodeGetLon n, nodeGetLat n)
        fWay _ = (0.0, 0.0)

indexedTags :: [String]
indexedTags = [
      "name"
    , "amenity"
    , "highway"
    , "historic"
    , "shop"
    , "tourism"
    , "leisure"
    , "website"
--    , "wikipedia"

    , "place"
    , "memorial:type"
    , "contact:website"
    , "cuisine"
    , "operator"
    , "phone"]

filterIndexedTags :: [(String, String)] -> [(String, String)]
filterIndexedTags tags' = filter (\(k,_) -> k `elem` indexedTags) tags'
