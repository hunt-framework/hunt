{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- ----------------------------------------------------------------------------
{- |
  'Document' compression using Google's Snappy library:
    <https://code.google.com/p/snappy/>

  Haskell-Bindings:
    <http://hackage.haskell.org/package/snappy>

  Requires the Snappy C library:

    - source: <https://code.google.com/p/snappy/>

    - deb: @apt-get install libsnappy-dev@

    - rpm: @yum install libsnappy-devel@
-}
-- ----------------------------------------------------------------------------

module Hunt.Common.Document.Compression.Snappy
where

#if  __GLASGOW_HASKELL__ >= 770
import qualified Codec.Compression.Snappy.Lazy as ZIP
#endif

import           Control.Applicative   ((<$>))
import           Control.DeepSeq

import           Data.Binary           (Binary (..))
import qualified Data.Binary           as B
import qualified Data.ByteString.Lazy  as BL
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import           Data.Typeable

import           Hunt.Common.Document

-- ------------------------------------------------------------

-- | A snappy-compressed 'Document'.
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

-- | Create a 'CompressedDoc' using Snappy-compression.
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

#if  __GLASGOW_HASKELL__ >= 770
-- | 'Document' to 'CompressedDoc' conversion.
compress    :: Document -> CompressedDoc
compress    = mkCDoc . Short.toShort . BL.toStrict . ZIP.compress . B.encode

-- | 'CompressedDoc' to 'Document' conversion.
decompress  :: CompressedDoc -> Document
decompress  = B.decode . ZIP.decompress . BL.fromStrict . Short.fromShort . unCDoc

#else
#warning snappy is disabled if GHC < 7.7
-- | 'Document' to 'CompressedDoc' conversion.
compress    :: Document -> CompressedDoc
compress    = mkCDoc . Short.toShort . BL.toStrict . B.encode

-- | 'CompressedDoc' to 'Document' conversion.
decompress  :: CompressedDoc -> Document
decompress  = B.decode . BL.fromStrict . Short.fromShort . unCDoc
#endif

-- ------------------------------------------------------------
