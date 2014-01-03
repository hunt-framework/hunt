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

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import Data.Text
import Data.Functor
import Control.Applicative


import           Data.Aeson

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
    --parseJSON _ = return fail

autocomplete :: String -> String  -> IO (JsonResponse [String])
autocomplete server query = do
    d <- simpleHttp $ "http://"++ server ++ "/completion/" ++ query ++ "/20"
    Just r <- return $ decode d
    return r