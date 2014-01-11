{-# LANGUAGE OverloadedStrings #-}

module Holumbus.Server.Client where

import Control.Lens (over, both)

import qualified Network.HTTP.Conduit as HTTP
--import qualified Data.ByteString.Lazy as L
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as TE
--import qualified Data.Text.Lazy.Encoding as T
--import Data.Functor
--import Control.Applicative

import Control.Monad (mzero, (>=>))
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.ByteString.Lazy (ByteString) 
import qualified Data.ByteString.Lazy as BL
import Data.Either ()
import Data.Char (isSpace)
import Data.String ()

data JsonResponse r = 
    JsonSuccess {jsonValue :: r}
    | JsonFailure Int [Text]
    deriving (Show)



instance (ToJSON r) => ToJSON (JsonResponse r) where
    toJSON (JsonSuccess msg) = object
        [ "code"  .= (0 :: Int)
        , "msg"   .= msg
        ]

    toJSON (JsonFailure n msg) = object
        [ "code"  .= n
        , "msg"   .= msg
        ]

instance (FromJSON r) => FromJSON (JsonResponse r) where
    parseJSON (Object v) = do
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
    parseJSON (Object v) = do
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
    return $ (decodeEither >=> handleJsonResponse >=> filterByRest >=> prefixWith) d
    where
        (rest, prefix) = over both T.reverse $ T.span (\ c -> (not $ isSpace c) && (c /= ':') && (c /= '!') && (c /= '~')) $ T.reverse q

        filterByRest :: [(Text, [Text])] -> Either Text [Text]
        filterByRest = return . map fst . filter (\(_, rs) -> rest `elem` rs)

        prefixWith :: [Text] -> Either Text [Text]
        prefixWith = ((return .) . fmap) $ (prefix `T.append`)
        

query :: (MonadIO m, FromJSON r) => Text -> Text -> m (Either Text (LimitedResult r))
query server q = do
    d <- HTTP.simpleHttp $ T.unpack $ T.concat ["http://" , server, "/search/", q, "/0/20"]
    return $ (decodeEither >=> handleJsonResponse) d

handleJsonResponse :: (FromJSON b) => JsonResponse b -> Either Text b
handleJsonResponse (JsonSuccess r) = Right r
handleJsonResponse (JsonFailure c err) = Left $ T.concat $ ["Code: ", T.pack $ show c, " "] ++ err

decodeEither :: (FromJSON b) => ByteString -> Either Text b
decodeEither j = 
    case decode j of
        Nothing -> Left ("failed to parse JSON " `T.append` (T.take 20 $ T.fromStrict $ TE.decodeUtf8 $ BL.toStrict j))
        Just d -> Right d