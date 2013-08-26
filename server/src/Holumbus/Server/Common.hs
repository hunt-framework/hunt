module Holumbus.Server.Common where

import           Control.Monad         (mzero)
import           Data.Monoid           (mappend)

import           Data.Aeson
import           Data.Map              (Map ())
import qualified Data.Map              as M
import           Data.Text             (Text)

import           Holumbus.Index.Common (Content, Context, Description, Position,
                                        URI, WordList)

-- ----------------------------------------------------------------------------

-- | Multiple ApiDocuments.
type ApiDocuments = [ApiDocument]

-- | The document accepted via the API.
data ApiDocument  = ApiDocument
  { apiDocUri       :: URI
  , apiDocIndexMap  :: Map Context IndexData
  , apiDocDescrMap  :: Description
  }

-- XXX: proper names
-- XXX: maybe as a real Either
-- custom either for convenient json parsing
data IndexData
  = ProcessedIndexData  TextData
  | RawIndexData        WordList

-- | Data necessary for adding documents to the index.
data TextData = TextData
  { idContent       :: Content
  , idMetadata      :: IndexMetadata
  }

-- | Metadata for index processing
data IndexMetadata = IndexMetadata
  { imAnalyzer :: AnalyzerType
  }


-- | Text analysis function
type AnalyzerFunction = Text -> [(Position, Text)]

-- | Types of analyzer
data AnalyzerType
  = DefaultAnalyzer

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

instance (ToJSON x) => ToJSON (PagedResult x) where
   toJSON (PagedResult l p pp c) = object
    [ "result"  .= l
    , "page"    .= p
    , "perPage" .= pp
    , "count"   .= c
    ]

-- | empty document
emptyApiDoc :: ApiDocument
emptyApiDoc = ApiDocument "" M.empty M.empty

instance FromJSON ApiDocument where
  parseJSON (Object o) = do
    parsedUri         <- o    .: "uri"
    indexMap          <- o    .: "index"
    descrMap          <- o    .: "description"
    return ApiDocument
      { apiDocUri       = parsedUri
      , apiDocIndexMap  = indexMap
      , apiDocDescrMap  = descrMap
      }
  parseJSON _ = mzero

instance FromJSON IndexData where
  parseJSON o =
    (do
      a <- parseJSON o
      return $ ProcessedIndexData a
    )
    `mappend`
    (do
      b <- parseJSON o
      return $ RawIndexData b
    )

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

instance ToJSON IndexData where
  toJSON (ProcessedIndexData a) = toJSON a
  toJSON (RawIndexData       a) = toJSON a

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


-- |  some sort of json response format
data JsonResponse r = JsonSuccess r | JsonFailure [Text]

instance (ToJSON r) => ToJSON (JsonResponse r) where
  toJSON (JsonSuccess msg) = object
    [ "code"  .= (0 :: Int)
    , "msg"   .= msg
    ]

  toJSON (JsonFailure msg) = object
    [ "code"  .= (1 :: Int)
    , "msg"   .= msg
    ]

-- ----------------------------------------------------------------------------

-- | 'Prelude.either' function for IndexData.
indexDataEither :: (WordList -> a) -> (TextData -> a) -> IndexData -> a
indexDataEither f1 f2 d = case d of
  RawIndexData       x -> f1 x
  ProcessedIndexData x -> f2 x
