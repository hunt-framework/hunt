{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}

module Hunt.Server.Common where

import           Data.Data     (Data)
import           Data.Typeable (Typeable)

import           Data.Aeson

-- ----------------------------------------------------------------------------

-- |  some sort of json response format
data JsonResponse r f
  = ToJSON r => JsonSuccess     r
  | ToJSON f => JsonFailure Int f

instance ToJSON (JsonResponse r f) where
  toJSON (JsonSuccess msg) = object
    [ "code"  .= (0 :: Int)
    , "msg"   .= msg
    ]

  toJSON (JsonFailure n msg) = object
    [ "code"  .= n
    , "msg"   .= msg
    ]

data HuntServerConfiguration = HuntServerConfiguration
  { huntServerHost     :: String
  , huntServerPort     :: Int
  , readIndexOnStartup :: Maybe FilePath
  } deriving (Show, Data, Typeable)
