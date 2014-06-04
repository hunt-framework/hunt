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

import           Control.Applicative
import           Control.DeepSeq

import           Data.Aeson
import           Data.Binary             (Binary (..))
import           Data.Text               as T
import           Data.Text.Binary        ()

import           Hunt.Common.ApiDocument
import           Hunt.Common.BasicTypes
import qualified Hunt.Common.DocDesc     as DD
import           Hunt.Utility.Log

-- ------------------------------------------------------------

-- | The document representation.
data Document = Document
  { uri   :: ! URI         -- ^ Unique identifier of the document.
  , desc  :: ! Description -- ^ Description of the document (simple key-value store).
  , wght  :: ! Score       -- ^ Weight used in ranking (default @1.0@).
  , score :: ! Score       -- ^ Score of a document in a result set
  }
  deriving (Show, Eq, Ord)

emptyDocument :: Document
emptyDocument = Document "" DD.empty defScore defScore

-- ------------------------------------------------------------
-- JSON instances implemented with ApiDocument
-- ------------------------------------------------------------

toApiDocument :: Document -> ApiDocument
toApiDocument (Document u d w s)
    = ApiDocument u emptyApiDocIndexMap d (fromDefScore w) (fromDefScore s)

fromApiDocument :: ApiDocument -> Document
fromApiDocument (ApiDocument u _ix d w s)
    = Document u d (toDefScore w) (toDefScore s)

setScore :: Score -> Document -> Document
setScore s d = d {score = s}

instance ToJSON Document where
    toJSON = toJSON . toApiDocument

instance FromJSON Document where
    parseJSON o = fromApiDocument <$> parseJSON o

-- ------------------------------------------------------------

instance Binary Document where
  put (Document u d w _) = put u >> put d >> put w
  get = Document <$> get <*> get <*> get <*> (pure defScore)

instance NFData Document where
  rnf (Document t d _w _s) = rnf t `seq` rnf d

-- ------------------------------------------------------------

-- | Simple bijection between @e@ and 'Document' for compression.
class DocumentWrapper e where
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
