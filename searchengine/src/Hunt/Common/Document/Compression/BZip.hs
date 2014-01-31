{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- ----------------------------------------------------------------------------
{-
  Document compression using the bzip2 library
    http://www.bzip.org/

  Haskell-Bindings
    http://hackage.haskell.org/package/bzlib
-}
-- ----------------------------------------------------------------------------

module Hunt.Common.Document.Compression.BZip
where

import qualified Codec.Compression.BZip   as ZIP

import           Control.Applicative      ((<$>))
import           Control.DeepSeq

import           Data.Binary              (Binary (..))
import qualified Data.Binary              as B
--import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BL
import           Data.ByteString.Short    (ShortByteString)
import qualified Data.ByteString.Short    as Short
import           Data.Typeable

import           Hunt.Common.Document

-- ----------------------------------------------------------------------------
-- TODO:
-- The CompressedDoc is a candidate for a BS.ShortByteString available with bytestring 0.10.4,
-- then 5 machine words can be saved per value

newtype CompressedDoc
  = CDoc { unCDoc :: ShortByteString }
    deriving (Eq, Show, NFData, Typeable)

mkCDoc :: ShortByteString -> CompressedDoc
mkCDoc v = CDoc $!! v

-- ----------------------------------------------------------------------------

instance Binary CompressedDoc where
    put = put . Short.fromShort . unCDoc
    get = mkCDoc . Short.toShort <$> get

-- to avoid sharing the data with the input, the ByteString is physically copied
-- before return. This should be the single place where sharing is introduced,
-- else the copy must be moved to mkCDoc

-- ----------------------------------------------------------------------------

instance DocumentWrapper CompressedDoc where
  wrap   = compress
  unwrap = decompress

-- ----------------------------------------------------------------------------

-- | 'Document' to 'CompressedDoc' conversion.
compress :: Document -> CompressedDoc
compress = mkCDoc . Short.toShort . BL.toStrict . ZIP.compress . B.encode

-- | 'CompressedDoc' to 'Document' conversion.
decompress  :: CompressedDoc -> Document
decompress = B.decode . ZIP.decompress . BL.fromStrict . Short.fromShort . unCDoc

-- ----------------------------------------------------------------------------
