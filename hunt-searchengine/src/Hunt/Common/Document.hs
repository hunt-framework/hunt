{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Hunt.Index.Common.Document
  Copyright  : Copyright (C) 2011 Sebastian M. Schlatt, Timo B. Huebel, Uwe Schmidt
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: none portable

  The Document datatype

-}

-- ----------------------------------------------------------------------------

module Hunt.Common.Document
where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad          (mzero)

import           Data.Aeson
import           Data.Binary            (Binary (..))
import           Data.Text              as T
import           Data.Text.Binary       ()

import           Hunt.Common.BasicTypes
import           Hunt.Utility
import           Hunt.Utility.Log

-- ------------------------------------------------------------

-- | A document consists of its unique identifier (URI).
data Document = Document
  { uri  :: ! URI
  , desc :: ! Description
  , wght :: ! Float
  }
  deriving (Show, Eq, Ord)

-- ------------------------------------------------------------

instance ToJSON Document where
  toJSON (Document u d w) = object' $
    [ "uri"    .== u
    , "desc"   .== d
    , "weight" .=? w .\. (== 1.0)
    ]

instance FromJSON Document where
  parseJSON (Object o) = do
    parsedDesc <- o .: "desc"
    parsedUri  <- o .: "uri"
    parsedWght <- o .: "weight"
    return Document
      { uri  = parsedUri
      , desc = parsedDesc
      , wght = parsedWght
      }
  parseJSON _ = mzero

-- ------------------------------------------------------------

instance Binary Document where
  put (Document u d w) = put u >> put d >> put w
  get = Document <$> get <*> get <*> get

instance NFData Document where
  rnf (Document t d w) = rnf t `seq` rnf d `seq` rnf w

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

-- ------------------------------------------------------------
