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

#if  __GLASGOW_HASKELL__ >= 770
import qualified Codec.Compression.Snappy.Lazy  as ZIP
#endif

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

instance DocumentWrapper CompressedDoc where
  wrap   = compress
  unwrap = decompress

-- ----------------------------------------------------------------------------

#if  __GLASGOW_HASKELL__ >= 770
-- | 'Document' to 'CompressedDoc' conversion.
compress    :: Document -> CompressedDoc
compress    = mkCDoc . ZIP.compress . B.encode

-- | 'CompressedDoc' to 'Document' conversion.
decompress  :: CompressedDoc -> Document
decompress  = B.decode . ZIP.decompress . unCDoc

#else
#warning snappy is disabled if GHC < 7.7
-- | 'Document' to 'CompressedDoc' conversion.
compress    :: Document -> CompressedDoc
compress    = mkCDoc . B.encode

-- | 'CompressedDoc' to 'Document' conversion.
decompress  :: CompressedDoc -> Document
decompress  = B.decode . unCDoc

#endif
