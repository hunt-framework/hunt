module Holumbus.Server.Common where

import Data.Aeson
import Data.Text as Text

-- |  some sort of json response format
data JsonResponse r = JsonSuccess r | JsonFailure Int [Text]

instance (ToJSON r) => ToJSON (JsonResponse r) where
  toJSON (JsonSuccess msg) = object
    [ "code"  .= (0 :: Int)
    , "msg"   .= msg
    ]

  toJSON (JsonFailure n msg) = object
    [ "code"  .= n
    , "msg"   .= msg
    ]
