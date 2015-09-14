{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- ----------------------------------------------------------------------------

{- |
  The document representation.

  This includes the

  * URI for identification,
  * the description for the data itself
  * the weight used in ranking and
  * optionally a score
-}

-- ----------------------------------------------------------------------------

module Hunt.Common.Document
where

import           Control.DeepSeq

import           Data.Aeson
import           Data.Binary             (Binary (..))
import           Data.Text               as T
import           Data.Text.Binary        ()

import           Hunt.Common.ApiDocument
import           Hunt.Common.BasicTypes
import qualified Hunt.Common.DocDesc     as DD
import           Hunt.Scoring.Score      (Score, defScore, fromDefScore, toDefScore)
import           Hunt.Utility.Log

-- ------------------------------------------------------------

-- | The document representation.
data Document = Document
  { uri   :: ! URI         -- ^ Unique identifier of the document.
  , desc  :: ! Description -- ^ Description of the document (simple key-value store).
  , wght  :: ! Score       -- ^ Weight used in ranking (default @1.0@).
  }
  deriving (Show, Eq)

emptyDocument :: Document
emptyDocument = Document "" DD.empty defScore

-- ------------------------------------------------------------
-- JSON instances implemented with ApiDocument
-- ------------------------------------------------------------

toApiDocument :: Document -> ApiDocument
toApiDocument (Document u d w)
    = ApiDocument u emptyApiDocIndexMap d (fromDefScore w)

fromApiDocument :: ApiDocument -> Document
fromApiDocument (ApiDocument u _ix d w)
    = Document u d (toDefScore w)

instance ToJSON Document where
    toJSON = toJSON . toApiDocument

instance FromJSON Document where
    parseJSON o = fromApiDocument <$> parseJSON o

-- ------------------------------------------------------------

instance Binary Document where
  put (Document u d w) = put u >> put d >> put w
  get = Document <$> get <*> get <*> get

instance NFData Document where
  rnf (Document t d _w) = rnf t `seq` rnf d

-- ------------------------------------------------------------

-- | Simple bijection between @e@ and 'Document' for compression.
class (NFData e) => DocumentWrapper e where
  -- | Get the document.
  unwrap :: e -> Document
  -- | Create e from document.
  wrap   :: Document -> e
  -- | Update the wrapped document.
  -- @update f = wrap . f . unwrap@.
  update :: (Document -> Document) -> e -> e
  update f = wrap . f . unwrap

instance DocumentWrapper Document where
  unwrap = id
  wrap   = id

-- ------------------------------------------------------------

instance LogShow Document where
  logShow o = "Document {uri = \"" ++ (T.unpack . uri $ o) ++ "\", ..}"

-- ------------------------------------------------------------
