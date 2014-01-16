{-# LANGUAGE OverloadedStrings #-}

module Holumbus.Common.ApiDocument where

import           Control.Applicative
import           Control.Monad              (mzero)

import           Data.Binary                (Binary (..))
import           Data.Text.Binary           ()
import           Data.Aeson
import           Data.Map                   (Map ())
import qualified Data.Map                   as M
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Holumbus.Common.BasicTypes

import           Holumbus.Utility.Log

-- ----------------------------------------------------------------------------

-- | Multiple ApiDocuments.
type ApiDocuments = [ApiDocument]

-- | The document accepted via the API.
data ApiDocument  = ApiDocument
  { apiDocUri      :: URI
  , apiDocIndexMap :: Map Context Content
  , apiDocDescrMap :: Description
  }
  deriving (Show)

-- | Text analysis function
type AnalyzerFunction = Text -> [(Position, Text)]

-- | Types of analyzer
data AnalyzerType
  = DefaultAnalyzer
  deriving (Show)

-- | paged api document result
data LimitedResult x = LimitedResult
  { lrResult :: [x]
  , lrOffset :: Int
  , lrMax    :: Int
  , lrCount  :: Int
  }
  deriving (Show, Eq)

-- ----------------------------------------------------------------------------

mkLimitedResult :: Int -> Int -> [x] -> LimitedResult x
mkLimitedResult offset mx xs = LimitedResult
  { lrResult = take mx . drop offset $ xs
  , lrOffset = offset
  , lrMax    = mx
  , lrCount  = length xs
  }

-- | empty document
emptyApiDocIndexMap :: Map Context Content
emptyApiDocIndexMap = M.empty

emptyApiDocDescrMap :: Description
emptyApiDocDescrMap = M.empty

emptyApiDoc :: ApiDocument
emptyApiDoc = ApiDocument "" emptyApiDocIndexMap emptyApiDocDescrMap

-- ----------------------------------------------------------------------------

instance Binary ApiDocument where
  put (ApiDocument a b c) = put a >> put b >> put c
  get = ApiDocument <$> get <*> get <*> get

-- ----------------------------------------------------------------------------

instance LogShow ApiDocument where
  logShow o = "ApiDocument {apiDocUri = \"" ++ (T.unpack . apiDocUri $ o) ++ "\", ..}"

-- ----------------------------------------------------------------------------

instance (ToJSON x) => ToJSON (LimitedResult x) where
   toJSON (LimitedResult res offset mx cnt) = object
    [ "result" .= res
    , "offset" .= offset
    , "max"    .= mx
    , "count"  .= cnt
    ]

instance FromJSON ApiDocument where
  parseJSON (Object o) = do
    parsedUri         <- o    .: "uri"
    indexMap          <- o    .:? "index"       .!= emptyApiDocIndexMap
    descrMap          <- o    .:? "description" .!= emptyApiDocDescrMap
    return ApiDocument
      { apiDocUri       = parsedUri
      , apiDocIndexMap  = indexMap
      , apiDocDescrMap  = descrMap
      }
  parseJSON _ = mzero

instance FromJSON AnalyzerType where
  parseJSON (String s) =
    case s of
      "default" -> return DefaultAnalyzer
      _         -> mzero
  parseJSON _ = mzero

instance ToJSON ApiDocument where
  toJSON (ApiDocument u im dm) = object
    [ "uri"         .= u
    , "index"       .= im
    , "description" .= dm
    ]

instance ToJSON AnalyzerType where
  toJSON (DefaultAnalyzer) =
    "default"
