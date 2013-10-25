{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- ----------------------------------------------------------------------------

module Holumbus.Common.CompressedDocument
where

import qualified Codec.Compression.BZip         as BZ

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

decompress  :: CompressedDoc -> Document
decompress  = B.decode . BZ.decompress . unCDoc

-- | 'Document' to 'CompressedDoc' conversion.
compress    :: Document -> CompressedDoc
compress    = CDoc . BZ.compress . B.encode

instance DocumentWrapper CompressedDoc where
  wrap   = compress
  unwrap = decompress

-- ----------------------------------------------------------------------------
