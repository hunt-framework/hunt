{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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

module Holumbus.Common.Document
where

import           Control.DeepSeq
import           Control.Monad              (liftM2, mzero)

import           Data.Aeson
import           Data.Binary                (Binary (..))
import           Data.Text                  as T
import           Data.Text.Binary           ()

import           Holumbus.Common.BasicTypes
import           Holumbus.Utility.Log

-- ------------------------------------------------------------

-- | A document consists of its unique identifier (URI).
data Document = Document
    { uri  :: ! URI
    , desc :: ! Description
    }
    deriving (Show, Eq, Ord)

-- ------------------------------------------------------------

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

-- ------------------------------------------------------------

instance Binary Document where
    put (Document u d) = put u >> put d
    get                   = liftM2 Document get get

instance NFData Document where
    rnf (Document t d) = rnf t `seq` rnf d

-- ------------------------------------------------------------

class DocumentWrapper e where
  unwrap :: e -> Document
  wrap   :: Document -> e
  update :: (Document -> Document) -> e -> e
  update f = wrap . f . unwrap

instance DocumentWrapper Document where
  unwrap = id
  wrap   = id

-- ------------------------------------------------------------

instance LogShow Document where
  logShow o = "Document {uri = \"" ++ (T.unpack . uri $ o) ++ "\", ..}"
