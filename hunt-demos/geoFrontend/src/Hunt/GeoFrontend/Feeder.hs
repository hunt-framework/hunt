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
import qualified Hunt.Server.Client as H



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
indexedTags = map H.cxName tagIndizes

filterIndexedTags :: [(String, String)] -> [(String, String)]
filterIndexedTags tags' = filter (flip elem indexedTags' . fst) tags'
    where
        indexedTags' = map cs $ indexedTags

metaIndizes :: [H.ContextDescription]
metaIndizes =
    [
          H.def {H.cxName = "type"          , H.cxWeight = 0.1, H.cxDefault = False}
        , H.def {H.cxName = "position"                        , H.cxDefault = False, H.cxType = H.PositionContext}
        , H.def {H.cxName = "kind"          , H.cxWeight = 0.1, H.cxDefault = False}
    ]

tagIndizes :: [H.ContextDescription]
tagIndizes =
    [
          H.def {H.cxName = "name"          , H.cxWeight = 1.0}
        , H.def {H.cxName = "amenity"       , H.cxWeight = 0.5}
        , H.def {H.cxName = "highway"       , H.cxWeight = 0.5}
        , H.def {H.cxName = "shop"          , H.cxWeight = 0.5}
        , H.def {H.cxName = "operator"      , H.cxWeight = 0.3}
        , H.def {H.cxName = "leisure"       , H.cxWeight = 0.3}
        , H.def {H.cxName = "tourism"       , H.cxWeight = 0.3}
        , H.def {H.cxName = "cuisine"       , H.cxWeight = 0.3}
        , H.def {H.cxName = "historic"      , H.cxWeight = 0.3}
        , H.def {H.cxName = "place"         , H.cxWeight = 0.3}
        , H.def {H.cxName = "memorial:type" , H.cxWeight = 0.1, H.cxDefault = False}
        , H.def {H.cxName = "website"       , H.cxWeight = 0.1, H.cxDefault = False}
        , H.def {H.cxName = "contact:website", H.cxWeight = 0.1,H.cxDefault = False}
        , H.def {H.cxName = "phone"         , H.cxWeight = 0.1, H.cxDefault = False}
    ]

indizes :: [H.ContextDescription]
indizes = metaIndizes ++ tagIndizes

createIndexCommands :: [H.Command]
createIndexCommands = map H.descriptionToCmd indizes
