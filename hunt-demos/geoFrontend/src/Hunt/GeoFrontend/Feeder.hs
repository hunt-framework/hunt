-- {-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Hunt.GeoFrontend.Feeder where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Lens (over, mapped, both)

import Data.Maybe (isJust, fromJust)
import Data.List (intersect)
-- import qualified Data.Set as Set

import Data.String.Conversions (cs) -- , (<>)
import Data.Text (Text)
-- import qualified Data.Text as T

-- import qualified Data.ByteString.Char8 as B (unpack)
import qualified Data.ByteString.Lazy as BL

--import Data.Aeson (FromJSON, ToJSON, decode, encode, Object, object, toJSON, (.=))
import qualified Data.Aeson as JSON

import Data.Geo.OSM
import Data.Geo.OSM.NWRCommon

import Hunt.GeoFrontend.Common
import qualified Hunt.Server.Client as HC
import qualified Hunt.ClientInterface as H
import qualified Hunt.Interpreter.Command as HIC


writeJSON :: MonadIO m => FilePath -> m ()
writeJSON p = do
    nwrs <- readXML p
    -- let docs = Set.fromList  nwrs
    liftIO $ writeFile "map.json" $ cs $ JSON.encode $ map geoDocToHuntDoc nwrs



readXML :: MonadIO m => FilePath -> m [GeoDocument]
readXML p = do
    [osm] <- liftIO $ Data.Geo.OSM.readOsmFile p
    let nwrs = chGetNWR $ osmChildren osm
        namedNwrs = filter nwrHasName nwrs
        nwrs' = filter (\n -> (isJust $ asNode n) || (isJust $ asWay n)) namedNwrs
    return $ map nwrToGeoDocument nwrs'

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
        tags' = over (mapped.both) cs $ filterIndexedTags $ fmap tagToPair $ nwrCommonGetTags $ getCommon nwr
        nameTag :: Text
        nameTag = fromJust $ lookup "name" tags'
        (nwrLon, nwrLat) = nwrGetCoords nwr
        kind' = foldNodeWayRelation nwr (const Node) (const Way) undefined

nwrGetCoords :: NodeWayRelation -> (Double, Double)
nwrGetCoords nwr = foldNodeWayRelation nwr fNode fWay undefined
    where
        fNode n = over both read $ (nodeGetLon n, nodeGetLat n)
        fWay _ = (0.0, 0.0)

indexedTags :: [Text]
indexedTags = map HIC.icIContext tagIndizes

filterIndexedTags :: [(String, String)] -> [(String, String)]
filterIndexedTags tags' = filter (flip elem indexedTags' . fst) tags'
    where
        indexedTags' = map cs $ indexedTags

metaIndizes :: [H.Command]
metaIndizes =
    [
          H.cmdInsertContext  "type"           $ H.setCxWeight 0.1 $ H.setCxNoDefault $ H.mkSchema
        , H.cmdInsertContext  "position"                           $ H.setCxNoDefault $ H.setCxPosition $ H.mkSchema
        , H.cmdInsertContext  "kind"           $ H.setCxWeight 0.1 $ H.setCxNoDefault $ H.mkSchema
    ]

tagIndizes :: [H.Command]
tagIndizes =
    [
          H.cmdInsertContext  "name"           $ H.setCxWeight 1.0 $ H.mkSchema
        , H.cmdInsertContext  "amenity"        $ H.setCxWeight 0.5 $ H.mkSchema
        , H.cmdInsertContext  "highway"        $ H.setCxWeight 0.5 $ H.mkSchema
        , H.cmdInsertContext  "shop"           $ H.setCxWeight 0.5 $ H.mkSchema
        , H.cmdInsertContext  "operator"       $ H.setCxWeight 0.3 $ H.mkSchema
        , H.cmdInsertContext  "leisure"        $ H.setCxWeight 0.3 $ H.mkSchema
        , H.cmdInsertContext  "tourism"        $ H.setCxWeight 0.3 $ H.mkSchema
        , H.cmdInsertContext  "cuisine"        $ H.setCxWeight 0.3 $ H.mkSchema
        , H.cmdInsertContext  "historic"       $ H.setCxWeight 0.3 $ H.mkSchema
        , H.cmdInsertContext  "place"          $ H.setCxWeight 0.3 $ H.mkSchema
        , H.cmdInsertContext  "memorial:type"  $ H.setCxWeight 0.1 $ H.setCxNoDefault $ H.mkSchema
        , H.cmdInsertContext  "website"        $ H.setCxWeight 0.1 $ H.setCxNoDefault $ H.mkSchema
        , H.cmdInsertContext  "contact:website" $ H.setCxWeight 0.1 $ H.setCxNoDefault $ H.mkSchema
        , H.cmdInsertContext  "phone"          $ H.setCxWeight 0.1 $ H.setCxNoDefault $ H.mkSchema
    ]

indizes :: [H.Command]
indizes = metaIndizes ++ tagIndizes
