{-# LANGUAGE OverloadedStrings #-}

module Hunt.Common.ApiDocument where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad          (mzero)

import           Data.Aeson
import           Data.Binary            (Binary (..))
import           Data.Map.Strict        (Map ())
import qualified Data.Map.Strict        as M
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Binary       ()

import           Hunt.Common.BasicTypes

import           Hunt.Utility.Log

-- ----------------------------------------------------------------------------

-- | Multiple ApiDocuments.
type ApiDocuments = [ApiDocument]

-- | The document accepted via the API.
data ApiDocument  = ApiDocument
  { adUri   :: URI
  , adIndex :: Map Context Content
  , adDescr :: Description
  , adWght  :: Maybe Float
  }
  deriving (Show)


instance NFData ApiDocument where
  --default

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

emptyApiDocDescr :: Description
emptyApiDocDescr = M.empty

emptyApiDoc :: ApiDocument
emptyApiDoc = ApiDocument "" emptyApiDocIndexMap emptyApiDocDescr Nothing

-- ----------------------------------------------------------------------------

instance Binary ApiDocument where
  put (ApiDocument a b c d) = put a >> put b >> put c >> put d
  get = ApiDocument <$> get <*> get <*> get <*> get

-- ----------------------------------------------------------------------------

instance LogShow ApiDocument where
  logShow o = "ApiDocument {adUri = \"" ++ (T.unpack . adUri $ o) ++ "\", ..}"

-- ----------------------------------------------------------------------------

instance (ToJSON x) => ToJSON (LimitedResult x) where
   toJSON (LimitedResult res offset mx cnt) = object
    [ "result" .= res
    , "offset" .= offset
    , "max"    .= mx
    , "count"  .= cnt
    ]

instance (FromJSON x) => FromJSON (LimitedResult x) where
  parseJSON (Object v) = do
    res    <- v .: "result"
    offset <- v .: "offset"
    mx     <- v .: "max"
    cnt    <- v .: "count"
    return $ LimitedResult res offset mx cnt
  parseJSON _ = mzero

instance FromJSON ApiDocument where
  parseJSON (Object o) = do
    parsedUri <- o    .: "uri"
    indexMap  <- o    .:? "index"       .!= emptyApiDocIndexMap
    descrMap  <- o    .:? "description" .!= emptyApiDocDescr
    weight    <- o    .:? "weight"
    return ApiDocument
      { adUri    = parsedUri
      , adIndex  = indexMap
      , adDescr  = descrMap
      , adWght   = weight
      }
  parseJSON _ = mzero

instance ToJSON ApiDocument where
  toJSON (ApiDocument u im dm wt) = object $
    (maybe [] (\ w -> ["weight" .= w]) wt)
    ++
    (if M.null dm then [] else ["index"       .= im])
    ++
    (if M.null dm then [] else ["description" .= dm])
    ++
    [ "uri"         .= u
    ]

instance FromJSON AnalyzerType where
  parseJSON (String s) =
    case s of
      "default" -> return DefaultAnalyzer
      _         -> mzero
  parseJSON _ = mzero

instance ToJSON AnalyzerType where
  toJSON (DefaultAnalyzer) =
    "default"

-- ----------------------------------------------------------------------------
