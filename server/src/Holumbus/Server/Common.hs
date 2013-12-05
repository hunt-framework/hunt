{-# LANGUAGE GADTs #-}

module Holumbus.Server.Common where

import Data.Aeson

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
