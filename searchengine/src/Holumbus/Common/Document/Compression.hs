{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- ----------------------------------------------------------------------------

module Holumbus.Common.Document.Compression
where

-- http://hackage.haskell.org/package/bzlib
import qualified Codec.Compression.BZip         as ZIP
-- http://hackage.haskell.org/package/snappy
-- import qualified Codec.Compression.Snappy       as ZIP

import           Control.DeepSeq

import           Data.ByteString.Lazy           (ByteString)
import qualified Data.ByteString.Lazy           as BS

import           Data.Binary                    (Binary(..))
import qualified Data.Binary                    as B

import           Holumbus.Common.Document

-- ----------------------------------------------------------------------------

newtype CompressedDoc
  = CDoc { unCDoc :: ByteString }
    deriving (Eq, Show)

instance Binary CompressedDoc where
    put = B.put . unCDoc
    get = B.get >>= return . CDoc

instance NFData CompressedDoc where
    rnf (CDoc s)
        = BS.length s `seq` ()

-- ----------------------------------------------------------------------------

-- | 'CompressedDoc' to 'conversion' Document.
decompress  :: CompressedDoc -> Document
decompress  = B.decode . ZIP.decompress . unCDoc

-- | 'Document' to 'CompressedDoc' conversion.
compress    :: Document -> CompressedDoc
compress    = CDoc . ZIP.compress . B.encode

-- ----------------------------------------------------------------------------
