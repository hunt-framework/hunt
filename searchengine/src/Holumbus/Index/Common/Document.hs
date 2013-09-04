-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Common.Document
  Copyright  : Copyright (C) 2011 Sebastian M. Schlatt, Timo B. Huebel, Uwe Schmidt
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: none portable

  The Document datatype

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Common.Document
where

import           Control.Monad                    (liftM, liftM2, mzero)
import           Control.DeepSeq

import           Data.Map                         (Map)
import           Data.Text                        (Text)
import           Data.Text.Encoding               as TE
import           Data.Binary                      (Binary (..))
import           Data.Aeson

import           Holumbus.Index.Common.BasicTypes

-- ------------------------------------------------------------

-- | The description of a document is a generic key value map.
type Description  = Map Text Text

-- XXX: move + UTF-8 serialization?
instance Binary Text where
  put = put . encodeUtf8
  get = liftM decodeUtf8 get



doc :: DocumentWrapper e -> DocumentRaw
doc = _getDoc

setDoc :: DocumentRaw -> DocumentWrapper e -> DocumentWrapper e
setDoc = flip _setDoc

modDoc :: (DocumentRaw -> DocumentRaw) -> DocumentWrapper e -> DocumentWrapper e
modDoc f d = setDoc (f . doc $ d) d

-- | A 'Document' wrapper with getter and setter.
data DocumentWrapper e = DocumentWrapper
  { _getDoc  :: DocumentRaw
  , _setDoc  :: DocumentRaw -> DocumentWrapper e
  , _impl    :: e
  }

instance NFData e => NFData (DocumentWrapper e) where
  rnf = rnf . _impl

instance Binary (DocumentWrapper DocumentRaw) where
  put = put . _impl
  get = get >>= return . wrapDoc


-- | A basic wrapper.
wrapDoc   :: DocumentRaw -> DocumentWrapper DocumentRaw
wrapDoc d = DocumentWrapper
  { _getDoc = d
  , _setDoc = wrapDoc
  , _impl   = d
  }

-- | A document consists of its unique identifier (URI).
data DocumentRaw                = DocumentRaw
                                  { uri   :: ! URI
                                  , desc  :: ! Description
                                  }
                                  deriving (Show, Eq, Ord)

instance ToJSON DocumentRaw where
  toJSON (DocumentRaw u d) = object
    [ "uri"   .= u
    , "desc"  .= toJSON d
    ]


instance FromJSON DocumentRaw where
  parseJSON (Object o) = do
    parsedDesc      <- o    .: "desc"
    parsedUri       <- o    .: "uri"
    return DocumentRaw
      { uri     = parsedUri
      , desc    = parsedDesc
      }
  parseJSON _ = mzero


instance Binary DocumentRaw where
    put (DocumentRaw u d)          = put u >> put d
    get                         = liftM2 DocumentRaw get get

instance NFData DocumentRaw where
    rnf (DocumentRaw t d)        = rnf t `seq` rnf d