{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- ----------------------------------------------------------------------------
{- |
  'Document' compression using the bzip2 library
    http://www.bzip.org/

  Haskell-Bindings
    http://hackage.haskell.org/package/bzlib
-}
-- ----------------------------------------------------------------------------

module Hunt.Common.Document.Compression.BZip
where

import qualified Codec.Compression.BZip as ZIP

import           Control.Applicative    ((<$>))
import           Control.DeepSeq

import           Data.Binary            (Binary (..))
import qualified Data.Binary            as B
import qualified Data.ByteString.Lazy   as BL
import           Data.ByteString.Short  (ShortByteString)
import qualified Data.ByteString.Short  as Short
import           Data.Typeable

import           Hunt.Common.Document

-- ------------------------------------------------------------

-- | A bzip-compressed 'Document'.
--   Use 'mkCDoc' to create.
--   The Document can be retrieved with 'unwrap'.
--   The corresponding bijection is defined in 'DocumentWrapper'.
--
-- Using 'ShortByteString' saves 5 machine words per value.
-- It also eliminates issues with sharing and fragmentation due to 'BL.ByteString's being pinned.
-- <https://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Short.html#g:1>
newtype CompressedDoc
  = CDoc { unCDoc :: ShortByteString }
  deriving (Eq, Show, NFData, Typeable)

-- | Create a 'CompressedDoc' using bzip-compression.
--
-- Deeply evaluates the value.
mkCDoc :: ShortByteString -> CompressedDoc
mkCDoc v = CDoc $!! v

-- ------------------------------------------------------------

instance Binary CompressedDoc where
    put = put . Short.fromShort . unCDoc
    get = mkCDoc . Short.toShort <$> get

-- to avoid sharing the data with the input, the ByteString is physically copied
-- before return. This should be the single place where sharing is introduced,
-- else the copy must be moved to mkCDoc

-- ------------------------------------------------------------

instance DocumentWrapper CompressedDoc where
  wrap   = compress
  unwrap = decompress

-- ------------------------------------------------------------

-- | 'Document' to 'CompressedDoc' conversion.
compress :: Document -> CompressedDoc
compress = mkCDoc . Short.toShort . BL.toStrict . ZIP.compress . B.encode

-- | 'CompressedDoc' to 'Document' conversion.
decompress  :: CompressedDoc -> Document
decompress = B.decode . ZIP.decompress . BL.fromStrict . Short.fromShort . unCDoc

-- ------------------------------------------------------------
