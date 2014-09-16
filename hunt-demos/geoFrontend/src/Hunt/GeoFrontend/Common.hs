{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}



module Hunt.GeoFrontend.Common
(
    GeoFrontendConfiguration (..),
    OSMType (..),
    runGeoReader,
    geoDocToHuntDoc,
    autocomplete,
    insert,
    query,
    GeoServer (..),
    GeoDocument (..)
)

where

import           GHC.Generics (Generic)


import           Control.Arrow (second)

import           Control.Monad (mzero)

import           Control.Lens hiding ((.=))

import           Text.Parsec (parse)

import           Data.Typeable (Typeable)

import           Data.Data (Data)

import           Data.Text (Text)
import qualified Data.Text as T
-- import qualified Data.Text.Lazy as TL

import qualified Data.HashMap.Lazy as HML
import qualified Data.Map          as M    (Map (), fromList)

import           Data.Aeson (FromJSON, ToJSON, (.:), (.=)) --(.:?), (.!=), Object
import qualified Data.Aeson  as JSON
import qualified Data.Aeson.Types as JSON

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Reader (ReaderT, MonadReader, ask, runReaderT)

import qualified Hunt.Server.Client as HC
import qualified Hunt.ClientInterface as H
import qualified Hunt.Index.Schema.Normalize.Position as HP

data OSMType = Way | Node
    deriving (Show, Eq, Generic, Ord)

instance ToJSON OSMType where
--    toJSON = JSON.genericToJSON H.lowercaseConstructorsOptions

instance FromJSON OSMType where
--    parseJSON = JSON.genericParseJSON H.lowercaseConstructorsOptions

data GeoDocument = GeoDocument {
    osmId :: Integer,
    name :: Text,
    lon  :: Double,
    lat  :: Double,
    kind :: OSMType,
    tags :: [(Text, Text)]
} deriving (Eq, Ord)

geoDocToMap :: GeoDocument -> [(Text, Text)]
geoDocToMap d = otherTags ++ (map . second) ($ d) [("name", name), ("position", position), ("kind", fromShow . kind)]
    where
        position d' = ((fromShow $ lon d') `T.append` "-" `T.append` (fromShow $ lat d'))
        otherTags = map (uncurry (,)) $ tags d

geoDocIdToUri :: GeoDocument -> Text
geoDocIdToUri d = "osm://" `T.append` (fromShow $ osmId d)

geoDocToHuntDoc :: GeoDocument -> H.ApiDocument
geoDocToHuntDoc d = H.listToApiDoc (geoDocIdToUri d) (geoDocToMap d) (geoDocToMap d)

instance FromJSON GeoDocument where
    parseJSON (JSON.Object o) = do
        u <- o .: "uri"
        (JSON.Object descr) <- o .: "description"
        n  <- descr .: "name"
        k  <- descr .: "kind"
        p  <- (descr .: "position") :: JSON.Parser Text
        (lon', lat') <- case parse HP.position "(json)" (T.unpack p) of
            Left err -> fail $ show err
            Right res -> return res

        -- tags' <- JSON.parse v
        let tags' = HML.toList $ descr `HML.difference` (HML.fromList [("name", undefined), ("kind", undefined), ("position", undefined)])
        tags'' <- (mapM . traverse) JSON.parseJSON tags'
        let osmId' = read $ snd $ splitAt 6 u
        return $ GeoDocument (osmId') n lon' lat' k (tags'')


    parseJSON _ = mzero

instance ToJSON GeoDocument where
    toJSON d = JSON.object $
        [ "name"  .= name d
        , "lon"   .= lon d
        , "lat"   .= lat d
        , "kind"   .= kind d
        ] ++ tags'
        where
            tags' :: [JSON.Pair]
            tags' = map (second JSON.String) $ tags d

fromShow :: (Show a) => a -> Text
fromShow = T.pack . show


newtype GeoServer a = GeoServer { runGeoServer :: ReaderT HC.ServerAndManager IO a }
    deriving (Monad, MonadIO, MonadReader (HC.ServerAndManager))

geoServer :: MonadTrans t => GeoServer a -> t GeoServer a
geoServer = lift

runGeoReader :: GeoServer a -> HC.ServerAndManager -> IO a
runGeoReader = runReaderT . runGeoServer

withServerAndManager' :: HC.HuntConnectionT IO b -> GeoServer b
withServerAndManager' x = do
    sm <- ask
    --sm <- liftIO $ STM.readTVarIO var
    liftIO $ HC.withServerAndManager sm x

autocomplete :: Text -> GeoServer [Text]
autocomplete q = withServerAndManager' $ HC.getAutocomplete q

query :: Text -> GeoServer (H.LimitedResult GeoDocument)
query q = withServerAndManager' $ HC.getQuery q 20 0


insert :: [GeoDocument] -> GeoServer Text
insert docs = withServerAndManager' $ HC.postCommand $ H.cmdSequence $ map (H.cmdInsertDoc . geoDocToHuntDoc) docs


data GeoFrontendConfiguration = GeoFrontendConfiguration {
    geoFrontendHost :: String,
    geoFrontendPort :: Int,
    huntUrl :: String,
    loadIndex  :: Maybe FilePath
} deriving (Show, Data, Typeable)
