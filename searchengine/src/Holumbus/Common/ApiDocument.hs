{-# LANGUAGE FlexibleInstances #-}

module Holumbus.Common.ApiDocument where

import           Control.Monad              (mzero)

import           Data.Monoid                (mappend)

import           Data.Aeson
import           Data.Map                   (Map ())
import qualified Data.Map                   as M
import           Data.Text                  (Text)

import           Holumbus.Common.BasicTypes

-- ----------------------------------------------------------------------------

-- | Multiple ApiDocuments.
type ApiDocuments = [ApiDocument]

-- XXX: newtype for now
-- also takes care of orphaned json instance warning with (Either WordList TextData)
-- either replace by a plain type or create a new datatype with ... = WL WordList | TD TextData
newtype TextData = TextData Content
  deriving (Show)

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

instance FromJSON (Either WordList TextData) where
  parseJSON o =
    (parseJSON o >>= return . Left)
    `mappend`
    (parseJSON o >>= return . Right)

instance FromJSON TextData where
  parseJSON x = parseJSON x >>= return . TextData

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

instance ToJSON (Either WordList TextData) where
  toJSON = either toJSON toJSON

instance ToJSON TextData where
  toJSON (TextData c) = toJSON c

instance ToJSON AnalyzerType where
  toJSON (DefaultAnalyzer) =
    "default"
