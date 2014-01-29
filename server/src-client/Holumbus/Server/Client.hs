{-# LANGUAGE OverloadedStrings, FlexibleContexts, GeneralizedNewtypeDeriving, PackageImports #-}

module Holumbus.Server.Client (
    withHolumbusServer
    , HolumbusConnectionT ()
    , ServerAndManager (..)
    , newServerAndManager
    , withServerAndManager
    , autocomplete
    , query
    , lowercaseConstructorsOptions
    , LimitedResult (..) 
    )where

import Data.Either ()
import Data.Char (isSpace, {- toUpper, -}toLower)
import Data.String ()

import Control.Monad (mzero, (>=>))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import "mtl" Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)

import Data.Text (Text)
import qualified Data.Text as T
-- import qualified Data.Text.Encoding as TE

import Data.ByteString.Lazy (ByteString) 
-- import qualified Data.ByteString.Lazy as BL

import Data.Aeson (FromJSON, ToJSON, (.=), (.:))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON

import Control.Lens (over, both, _Left, {-_head, _tail, -}each, Mutator, Each)

import Data.Conduit (runResourceT, ResourceT, MonadBaseControl)
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Client as HTTP (defaultManagerSettings)

import Control.Failure (Failure)

data JsonResponse r = 
    JsonSuccess {_jsonValue :: r}
    | JsonFailure Int [Text]
    deriving (Show)



instance (ToJSON r) => ToJSON (JsonResponse r) where
    toJSON (JsonSuccess msg) = JSON.object
        [ "code"  .= (0 :: Int)
        , "msg"   .= msg
        ]

    toJSON (JsonFailure n msg) = JSON.object
        [ "code"  .= n
        , "msg"   .= msg
        ]

instance (FromJSON r) => FromJSON (JsonResponse r) where
    parseJSON (JSON.Object v) = do
        code <- v .: "code"   
        case code of 
            0 -> do
                msg <- v .: "msg"
                return $ JsonSuccess msg
            _ -> do
                msg <- v .: "msg"
                return $ JsonFailure code msg
    parseJSON _ = mzero

-- TODO: join with ApiDocument.hs
data LimitedResult x = LimitedResult
    { lrResult :: [x]
    , lrOffset :: Int
    , lrMax    :: Int
    , lrCount  :: Int
    }
    deriving (Show, Eq)

instance (FromJSON x) => FromJSON (LimitedResult x) where
    parseJSON (JSON.Object v) = do
        r <- v .: "result" 
        o <- v .: "offset"
        m <- v .: "max"
        c <- v .: "count"
        return $ LimitedResult r o m c
    parseJSON _ = mzero

-- ------------------------------
data ServerAndManager = ServerAndManager {
    unServer :: Text,
    unManager :: HTTP.Manager
}


newtype HolumbusConnectionT m a = HolumbusConnectionT { runHolumbusConnectionT :: ReaderT ServerAndManager (ResourceT m) a }
    deriving (Monad, MonadReader ServerAndManager, MonadIO)

instance MonadTrans HolumbusConnectionT where
    -- lift :: (Monad m) => m a -> HolumbusConnectionT m a
    lift = HolumbusConnectionT . lift . lift

-- | Creates a new ServerAndManager from a Host
newServerAndManager :: Text -> IO ServerAndManager
newServerAndManager s = do
    m <- HTTP.newManager HTTP.defaultManagerSettings
    return $ ServerAndManager s m

-- | runs a HolumbusConnectionT with a ServerAndManager
withServerAndManager :: MonadBaseControl IO m =>  HolumbusConnectionT m a -> ServerAndManager -> m a
withServerAndManager x = runResourceT . runReaderConnectionT x

-- | runs a HolumbusConnectionT in a monad
withHolumbusServer :: (MonadIO m, MonadBaseControl IO m) => HolumbusConnectionT m a -> Text -> m a
withHolumbusServer x s = HTTP.withManager (runReaderConnectionT x . ServerAndManager s)


runReaderConnectionT :: HolumbusConnectionT m a -> ServerAndManager -> ResourceT m a
runReaderConnectionT x sm = (runReaderT . runHolumbusConnectionT) x sm


makeRequest :: (MonadIO m, Failure HTTP.HttpException m) => Text -> HolumbusConnectionT m HTTP.Request
makeRequest path = do
    sm <- ask
    HTTP.parseUrl $ T.unpack $ T.concat [ "http://", unServer sm, path]

httpLbs :: (MonadIO m) => HTTP.Request -> HolumbusConnectionT m ByteString
httpLbs request = do
    sm <- ask
    response <- HTTP.httpLbs request (unManager sm)
    return $ HTTP.responseBody response



autocomplete :: (MonadIO m, Failure HTTP.HttpException m) => Text  -> HolumbusConnectionT m (Either Text [Text])
autocomplete q = do
    request <- makeRequest $ T.concat [ "/completion/", q, "/20"]
    d <- httpLbs request
    return $ (eitherDecodeT >=> handleJsonResponse >=> filterByRest >=> prefixWith) d
    where
        (rest, prefix) = over both T.reverse $ T.span (\ c -> (not $ isSpace c) && (c /= ':') && (c /= '!') && (c /= '~')) $ T.reverse q

        filterByRest :: [(Text, [Text])] -> Either Text [Text]
        filterByRest = return . map fst . filter (\(_, rs) -> rest `elem` rs)

        prefixWith :: [Text] -> Either Text [Text]
        prefixWith = ((return .) . fmap) $ (prefix `T.append`)
        

query :: (MonadIO m, FromJSON r, Failure HTTP.HttpException m) => Text -> HolumbusConnectionT m  (Either Text (LimitedResult r))
query q = do
    request <- makeRequest $ T.concat [ "/search/", q, "/0/20"]
    d <- httpLbs request
    return $ (eitherDecodeT >=> handleJsonResponse) d

handleJsonResponse :: (FromJSON b) => JsonResponse b -> Either Text b
handleJsonResponse (JsonSuccess r) = Right r
handleJsonResponse (JsonFailure c err) = Left $ T.concat $ ["Code: ", T.pack $ show c, " "] ++ err

eitherDecodeT :: FromJSON a => ByteString -> Either Text a
eitherDecodeT =  over _Left (("Json decode error: " `T.append`) . T.pack) . JSON.eitherDecode

--capitalize :: String -> String
--capitalize = over _head toUpper . over (_tail.each) toLower

lowercase :: Each Mutator s t Char Char => s -> t
lowercase = over (each) toLower

lowercaseConstructorsOptions :: JSON.Options
lowercaseConstructorsOptions = JSON.Options { 
       JSON.fieldLabelModifier      = id
     , JSON.constructorTagModifier  = lowercase
     , JSON.allNullaryToStringTag   = True
     , JSON.omitNothingFields       = False
     , JSON.sumEncoding             = JSON.defaultTaggedObject
 }