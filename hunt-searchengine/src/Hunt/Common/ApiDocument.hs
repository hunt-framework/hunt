{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- ----------------------------------------------------------------------------

{- |
  Document format for the interpreter and JSON API.
  It includes the document description, the index data and additional metadata.
-}

-- ----------------------------------------------------------------------------

module Hunt.Common.ApiDocument where


import           Control.DeepSeq
import           Control.Monad (mzero)
import           Data.Aeson
import           Data.Binary (Binary (..))
import           Data.Map.Strict (Map ())
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Binary ()
import           GHC.Generics
import           Hunt.Common.BasicTypes
import qualified Hunt.Common.DocDesc as DD
import           Hunt.Scoring.Score (Score, getScore, mkScore, noScore)
import           Hunt.Utility.Log

-- ------------------------------------------------------------

-- | The document accepted by the interpreter and JSON API.

data ApiDocument  = ApiDocument
    { adUri   :: URI              -- ^ The unique identifier.
    , adIndex :: IndexMap         -- ^ The data to index according to schema associated with the context.
    , adDescr :: Description      -- ^ The document description (a simple key-value map).
    , adWght  :: Score            -- ^ An optional document boost, (internal default is @1.0@).
    }
    deriving (Show, Generic)

-- | Context map
type IndexMap = Map Context Content

-- | Multiple 'ApiDocument's.
type ApiDocuments = [ApiDocument]

-- | Text analysis function
type AnalyzerFunction = Text -> [(Position, Text)]

-- | Types of analyzer
data AnalyzerType = DefaultAnalyzer
                    deriving (Show)


-- | Paginated result with an offset and chunk size.
data LimitedResult x = LimitedResult
    { lrResult :: [x] -- ^ The list with at most 'lrMax' elements.
    , lrOffset :: Int -- ^ The offset of the result.
    , lrMax    :: Int -- ^ The limit for the result.
    , lrCount  :: Int -- ^ The size of the complete result.
    }
    deriving (Show, Eq)

instance NFData x => NFData (LimitedResult x) where
  rnf (LimitedResult r o m c) = r `seq` o `seq` m `seq` c `seq` ()

instance Functor LimitedResult where
  fmap f lr = lr { lrResult = fmap f (lrResult lr) }

-- ------------------------------------------------------------

-- | Create a paginated result with an offset and a chunk size.
--   The result also includes the size of the complete result.

mkLimitedResult :: Int -> Int -> [x] -> LimitedResult x
mkLimitedResult offset mx xs = LimitedResult
  { lrResult = ( if mx < 0
                 then id
                 else take mx
               ) . drop offset $ xs
  , lrOffset = offset
  , lrMax    = mx
  , lrCount  = length xs
  }


-- | Empty index content.
emptyApiDocIndexMap :: IndexMap
emptyApiDocIndexMap = M.empty

-- | Empty 'Document' description.
emptyApiDocDescr :: Description
emptyApiDocDescr = DD.empty

-- | Empty 'ApiDocument'.
emptyApiDoc :: ApiDocument
emptyApiDoc = ApiDocument "" emptyApiDocIndexMap emptyApiDocDescr noScore

-- ------------------------------------------------------------

instance NFData ApiDocument where
  rnf x = seq x ()
  -- default =< ghc-7.8

-- ------------------------------------------------------------

instance Binary ApiDocument where
  put (ApiDocument a b c d)
      = put a >> put b >> put c >> put d
  get = ApiDocument <$> get <*> get <*> get <*> get

-- ------------------------------------------------------------

instance LogShow ApiDocument where
  logShow o = "ApiDocument {adUri = \"" ++ (T.unpack . adUri $ o) ++ "\", ..}"

-- ------------------------------------------------------------
-- JSON instances
-- ------------------------------------------------------------

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
    weight    <- mkScore <$>
                 o    .:? "weight"      .!= 0.0
    return ApiDocument
      { adUri    = parsedUri
      , adIndex  = indexMap
      , adDescr  = descrMap
      , adWght   = weight
      }
  parseJSON _ = mzero

instance ToJSON ApiDocument where
  toJSON (ApiDocument u im dm wt)
      = object $
        ( maybe [] (\ x -> ["weight" .= x]) $ getScore wt )
        ++
        ( if M.null im
          then []
          else ["index" .= im]
        )
        ++
        ( if DD.null dm
          then []
          else ["description" .= dm]
        )
        ++
        ["uri" .= u]

instance FromJSON AnalyzerType where
  parseJSON (String s) =
    case s of
      "default" -> return DefaultAnalyzer
      _         -> mzero
  parseJSON _ = mzero

instance ToJSON AnalyzerType where
  toJSON (DefaultAnalyzer) =
    "default"

-- ------------------------------------------------------------
