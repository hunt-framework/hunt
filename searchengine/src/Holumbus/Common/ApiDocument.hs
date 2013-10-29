{-# LANGUAGE FlexibleInstances #-}

module Holumbus.Common.ApiDocument where

import           Control.Monad                   (mzero)

import           Data.Monoid                     (mappend)

import           Data.Aeson
import           Data.Map                        (Map ())
import qualified Data.Map                        as M
import           Data.Text                       (Text)

import           Holumbus.Common.BasicTypes

-- ----------------------------------------------------------------------------

-- | Multiple ApiDocuments.
type ApiDocuments = [ApiDocument]

-- | The document accepted via the API.
data ApiDocument  = ApiDocument
  { apiDocUri      :: URI
  , apiDocIndexMap :: Map Context (Either WordList TextData)
  , apiDocDescrMap :: Description
  }
  deriving (Show)

-- | Data necessary for adding documents to the index.
data TextData = TextData
  { idContent  :: Content
  , idMetadata :: IndexMetadata
  }
  deriving (Show)

-- | Metadata for index processing
data IndexMetadata = IndexMetadata
  { imAnalyzer :: AnalyzerType
  }
  deriving (Show)

-- | Text analysis function
type AnalyzerFunction = Text -> [(Position, Text)]

-- | Types of analyzer
data AnalyzerType
  = DefaultAnalyzer
  deriving (Show)

  -- | The default Matadata
defaultIndexMetadata :: IndexMetadata
defaultIndexMetadata = IndexMetadata
  { imAnalyzer = DefaultAnalyzer
  }

-- | paged api document result
data PagedResult x = PagedResult
  { result  :: [x]
  , page    :: Int
  , perPage :: Int
  , count   :: Int
  }

mkPagedResult :: [x] -> Int -> Int -> PagedResult x
mkPagedResult xs p pp = PagedResult
  { result  = takePage
  , page    = p
  , perPage = pp
  , count   = length xs
  }
  where
  takePage = take pp $ drop (pp * (p-1)) xs

-- | empty document
emptyApiDocIndexMap :: Map Context (Either WordList TextData)
emptyApiDocIndexMap = M.empty

emptyApiDocDescrMap :: Description
emptyApiDocDescrMap = M.empty

emptyApiDoc :: ApiDocument
emptyApiDoc = ApiDocument "" emptyApiDocIndexMap emptyApiDocDescrMap

-- ----------------------------------------------------------------------------

instance (ToJSON x) => ToJSON (PagedResult x) where
   toJSON (PagedResult l p pp c) = object
    [ "result"  .= l
    , "page"    .= p
    , "perPage" .= pp
    , "count"   .= c
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
  parseJSON (Object o) = do
    content           <- o    .:  "content"
    metadata          <- o    .:? "metadata" .!= defaultIndexMetadata
    return TextData
      { idContent       = content
      , idMetadata      = metadata
      }
  parseJSON _ = mzero

instance FromJSON IndexMetadata where
  parseJSON (Object o) = do
    analyzer <- o .: "analyzer" .!= DefaultAnalyzer
    return IndexMetadata
      { imAnalyzer = analyzer
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

instance ToJSON (Either WordList TextData) where
  toJSON = either toJSON toJSON

instance ToJSON TextData where
  toJSON (TextData c m) = object
    [ "content"     .= c
    , "metadata"    .= m
    ]

instance ToJSON IndexMetadata where
  toJSON (IndexMetadata a) = object
    [ "analyzer"    .= a
    ]

instance ToJSON AnalyzerType where
  toJSON (DefaultAnalyzer) =
    "default"

-- ----------------------------------------------------------------------------
-- Interpreter
-- ----------------------------------------------------------------------------

-- TODO: improve this :D

