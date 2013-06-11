module Holumbus.Server.Common where

import           Control.Monad         (mzero)

import           Data.Aeson
import           Data.Map              (Map ())
import qualified Data.Map              as M
import           Data.Text

import           Holumbus.Index.Common

-- | map from context to a list of words with occurrences
type Words        = Map Context WordList

-- | map from word to a list of occurrences
type WordList     = Map Word [Position]

-- | type the server receives to add/modify/delete? in index
data ApiDocument = ApiDocument
  { apiDocUri   :: URI
  , apiDocDesc  :: Description
  , apiDocWords :: Words
  } deriving Show

-- | empty document
emptyApiDoc :: ApiDocument
emptyApiDoc = ApiDocument "" M.empty M.empty

instance ToJSON ApiDocument where
  toJSON (ApiDocument u d ws) = object
    [ "uri"   .= u
    , "desc"  .= toJSON d
    , "words" .= toJSON ws
    ]

instance FromJSON ApiDocument where
  parseJSON (Object o) = do
    parsedDesc      <- o    .: "desc"
    parsedUri       <- o    .: "uri"
    parsedWords     <- o    .: "words"
    return ApiDocument
      { apiDocUri     = parsedUri
      , apiDocDesc    = parsedDesc
      , apiDocWords   = parsedWords
      }
  parseJSON _ = mzero


type ApiDocuments = [ApiDocument]

-- |  some sort of json response format
data JsonResponse r = JsonSuccess r | JsonFailure Text

instance (ToJSON r) => ToJSON (JsonResponse r) where
  toJSON (JsonSuccess msg) = object
    [ "code"  .= (0 :: Int)
    , "msg"   .= toJSON msg
    ]

  toJSON (JsonFailure msg) = object
    [ "code"  .= (1 :: Int)
    , "msg"   .= msg
    ]



