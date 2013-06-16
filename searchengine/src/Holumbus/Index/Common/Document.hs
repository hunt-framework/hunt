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

import Control.Monad                    ( liftM, liftM2, mzero )
import Control.DeepSeq

import           Data.Map (Map)
import           Data.Text (Text)
import Data.Text.Encoding as TE
import Data.Binary                      ( Binary (..) )

import Holumbus.Index.Common.BasicTypes

import Text.XML.HXT.Core

import Data.Aeson
-- ------------------------------------------------------------

-- | A document consists of a title and its unique identifier (URI)
-- and a customizable component

-- FIXME: redundant/move?
type Attribute    = Text
type Description  = Map Attribute Text

-- XXX: move + UTF-8 serialization?
instance Binary Text where
  put = put . encodeUtf8
  get = liftM decodeUtf8 get

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


-- FIXME: remove -> using json now
instance XmlPickler Description where
    xpickle = undefined

-- FIXME: remove -> using json now
instance XmlPickler Document where
    xpickle = undefined
--    xpickle                     = xpWrap ( \ (t, d) -> Document t d
--                                         , \ (Document t d) -> (t, d)
--                                         ) (xpPair xpURI xpickle)
--        where
--        xpURI                   = xpAttr "href"  xpText0


instance NFData Document where
    rnf (Document t d)        = rnf t `seq` rnf d

-- ------------------------------------------------------------
