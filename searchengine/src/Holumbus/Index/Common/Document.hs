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

import Control.Monad                    ( liftM, liftM2 )
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

-- TODO: redundant/move?
type Attribute    = Text
type Description  = Map Attribute Text
type Words        = Map CContext WordList
type CContext     = Text
type WordList     = Map WWord [Int]
type WWord        = Text
type Uri          = Text

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


instance Binary Document where
    put (Document u d)          = put u >> put d
    get                         = liftM2 Document get get


-- FIXME: implement
instance XmlPickler Description where
    xpickle = undefined


instance XmlPickler Document where
    xpickle                     = xpWrap ( \ (t, d) -> Document t d
                                         , \ (Document t d) -> (t, d)
                                         ) (xpPair xpURI xpickle)
        where
        xpURI                   = xpAttr "href"  xpText0


instance NFData Document where
    rnf (Document t d)        = rnf t `seq` rnf d

-- ------------------------------------------------------------
