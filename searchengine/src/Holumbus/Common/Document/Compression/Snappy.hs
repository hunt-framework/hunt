-- ----------------------------------------------------------------------------
{-
  Document compression using Google's Snappy library
    https://code.google.com/p/snappy/

  Haskell-Bindings
    http://hackage.haskell.org/package/snappy

  Requires the Snappy C library
    source: https://code.google.com/p/snappy/
    deb: apt-get install libsnappy-dev
    rpm: yum install libsnappy-devel
-}
-- ----------------------------------------------------------------------------

module Holumbus.Common.Document.Compression.Snappy
where

import qualified Codec.Compression.Snappy.Lazy  as ZIP

import           Control.DeepSeq

import           Data.ByteString.Lazy           (ByteString)
import qualified Data.ByteString.Lazy           as BL

import           Data.Binary                    (Binary(..))
import qualified Data.Binary                    as B

import           Holumbus.Common.Document

-- ----------------------------------------------------------------------------

newtype CompressedDoc
  = CDoc { unCDoc :: ByteString }
    deriving (Eq, Show)

mkCDoc :: ByteString -> CompressedDoc
mkCDoc v = CDoc $!! v

-- ----------------------------------------------------------------------------

instance Binary CompressedDoc where
    put = B.put . unCDoc
    get = B.get >>= return . mkCDoc

instance NFData CompressedDoc where
    rnf (CDoc s)
        = BL.length s `seq` ()

-- ----------------------------------------------------------------------------

-- | 'CompressedDoc' to 'Document' conversion.
decompress  :: CompressedDoc -> Document
decompress  = B.decode . ZIP.decompress . unCDoc

-- | 'Document' to 'CompressedDoc' conversion.
compress    :: Document -> CompressedDoc
compress    = mkCDoc . ZIP.compress . B.encode

-- ----------------------------------------------------------------------------

instance DocumentWrapper CompressedDoc where
  wrap   = compress
  unwrap = decompress
