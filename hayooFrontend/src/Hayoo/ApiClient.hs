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
import Data.Text
--import Data.Functor
--import Control.Applicative
import Control.Monad


import Data.Aeson

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
    functionDescription :: Text
} deriving (Show, Eq)

instance FromJSON SearchResult where
    parseJSON (Object v) = do
        u <- v .: "uri" 
        (Object descr) <- v .: "desc"
        p  <- descr .: "function-package"
        m  <- descr .: "function-module"
        n  <- descr .: "function-name"
        s  <- descr .: "function-signature"
        d  <- descr .: "function-description"
        return $ FunctionResult u p m n s d
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


autocomplete :: String -> String  -> IO (JsonResponse [String])
autocomplete server query = do
    d <- HTTP.simpleHttp $ "http://"++ server ++ "/completion/" ++ query ++ "/20"
    Just r <- return $ decode d
    return r

query :: String -> String -> IO (JsonResponse (LimitedResult SearchResult))
query server query = do
    d <- HTTP.simpleHttp $ "http://" ++ server ++ "/search/" ++ query ++ "/0/20"
    Just r <- return $ decode d
    return r
