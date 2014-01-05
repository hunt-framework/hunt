{-# LANGUAGE OverloadedStrings #-}

module Hayoo.ApiClient where

{-
import qualified Network.HTTP.Client as HTTP

newManager :: String -> IO HTTP.Manager
newManager host = do
    HTTP.newManager HTTP.defaultManagerSettings

autocomplete :: String -> HTTP.Manager-> IO [String]
autocomplete w = do
-}

import qualified Network.HTTP.Conduit as HTTP
--import qualified Data.ByteString.Lazy as L
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
--import qualified Data.Text.Lazy.Encoding as T
--import Data.Functor
--import Control.Applicative

import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.String (IsString)
import Data.ByteString.Lazy (ByteString) 
import Data.Either ()

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

data SearchResult = FunctionResult {
    uri :: Text, 
    functionPackage :: Text,
    functionModule :: Text,
    functionName :: Text,
    functionSignature :: Text,
    functionDescription :: Text,
    functionSource :: Text
} deriving (Show, Eq)

instance FromJSON SearchResult where
    parseJSON (Object v) = do
        u <- v .: "uri" 
        (Object descr) <- v .: "desc"
        p  <- descr .: "package"
        m  <- descr .: "module"
        n  <- descr .: "name"
        s  <- descr .: "signature"
        d  <- descr .:? "description" .!= ""
        c  <- descr .:? "source" .!= ""
        return $ FunctionResult u p m n s d c
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

-- autocomplete :: String -> String  -> IO (JsonResponse [String])
-- autocomplete:: forall (m :: * -> *) b. (MonadIO m, FromJSON b) => [Char] -> [Char] -> m b
autocomplete :: (MonadIO m) => Text -> Text  -> m (Either Text [Text])
autocomplete server q = do
    d <- HTTP.simpleHttp $ T.unpack $ T.concat [ "http://", server, "/completion/", q, "/20"]
    return $ (decodeEither >=> handleJsonResponse) d

query :: (MonadIO m) => Text -> Text -> m (Either Text (LimitedResult SearchResult))
query server q = do
    d <- HTTP.simpleHttp $ T.unpack $ T.concat ["http://" , server, "/search/", q, "/0/20"]
    return $ (decodeEither >=> handleJsonResponse) d

handleJsonResponse :: (FromJSON b) => JsonResponse b -> Either Text b
handleJsonResponse (JsonSuccess r) = Right r
handleJsonResponse (JsonFailure c err) = Left $ T.concat $ ["Code: ", T.pack $ show c, " "] ++ err

decodeEither :: (IsString a, FromJSON b) => ByteString -> Either a b
decodeEither j = 
    case decode j of
        Nothing -> Left "failed to parse JSON"
        Just d -> Right d