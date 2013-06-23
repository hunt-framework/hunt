{-# OPTIONS #-}

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

-- | A document consists of its unique identifier (URI).
data Document                   = Document
                                  { uri   :: ! URI
                                  , desc  :: ! Description
                                  }
                                  deriving (Show, Eq, Ord)

instance ToJSON Document where
  toJSON (Document u d) = object
    [ "uri"   .= u
    , "desc"  .= toJSON d
    ]


instance FromJSON Document where
  parseJSON (Object o) = do
    parsedDesc      <- o    .: "desc"
    parsedUri       <- o    .: "uri"
    return Document
      { uri     = parsedUri
      , desc    = parsedDesc
      }
  parseJSON _ = mzero


instance Binary Document where
    put (Document u d)          = put u >> put d
    get                         = liftM2 Document get get

instance NFData Document where
    rnf (Document t d)        = rnf t `seq` rnf d

