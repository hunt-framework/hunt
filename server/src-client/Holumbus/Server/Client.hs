{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Holumbus.Server.Client where

import Data.Either ()
import Data.Char (isSpace, toUpper, toLower)
import Data.String ()

import Control.Monad (mzero, (>=>))
import Control.Monad.IO.Class (MonadIO)

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
-- import qualified Data.Text.Encoding as TE

import Data.ByteString.Lazy (ByteString) 
-- import qualified Data.ByteString.Lazy as BL

import Data.Aeson (FromJSON, ToJSON, (.=), (.:))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON

import Control.Lens (over, both, _Left, _head, _tail, each, Mutator, Each)

import qualified Network.HTTP.Conduit as HTTP


data JsonResponse r = 
    JsonSuccess {jsonValue :: r}
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

autocomplete :: (MonadIO m) => Text -> Text  -> m (Either Text [Text])
autocomplete server q = do
    d <- HTTP.simpleHttp $ T.unpack $ T.concat [ "http://", server, "/completion/", q, "/20"]
    return $ (eitherDecodeT >=> handleJsonResponse >=> filterByRest >=> prefixWith) d
    where
        (rest, prefix) = over both T.reverse $ T.span (\ c -> (not $ isSpace c) && (c /= ':') && (c /= '!') && (c /= '~')) $ T.reverse q

        filterByRest :: [(Text, [Text])] -> Either Text [Text]
        filterByRest = return . map fst . filter (\(_, rs) -> rest `elem` rs)

        prefixWith :: [Text] -> Either Text [Text]
        prefixWith = ((return .) . fmap) $ (prefix `T.append`)
        

query :: (MonadIO m, FromJSON r) => Text -> Text -> m (Either Text (LimitedResult r))
query server q = do
    d <- HTTP.simpleHttp $ T.unpack $ T.concat ["http://" , server, "/search/", q, "/0/20"]
    return $ (eitherDecodeT >=> handleJsonResponse) d

handleJsonResponse :: (FromJSON b) => JsonResponse b -> Either Text b
handleJsonResponse (JsonSuccess r) = Right r
handleJsonResponse (JsonFailure c err) = Left $ T.concat $ ["Code: ", T.pack $ show c, " "] ++ err

eitherDecodeT :: FromJSON a => ByteString -> Either Text a
eitherDecodeT =  over _Left (("Json decode error: " `T.append`) . T.pack) . JSON.eitherDecode

capitalize :: String -> String
capitalize = over _head toUpper . over (_tail.each) toLower

lowercase :: Each Mutator s t Char Char => s -> t
lowercase = over (each) toLower

-- lowercaseConstructorsOptions :: JSON.Options
lowercaseConstructorsOptions = JSON.Options { 
     JSON.fieldLabelModifier      = id
     , JSON.constructorTagModifier  = lowercase
     , JSON.allNullaryToStringTag   = True
     , JSON.omitNothingFields       = False
     , JSON.sumEncoding             = JSON.defaultTaggedObject
 }