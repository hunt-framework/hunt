module Holumbus.Index.Common.CompressedDocument
where


import qualified Codec.Compression.BZip         as BZ

import           Data.ByteString.Lazy           (ByteString)
import qualified Data.ByteString.Lazy           as BS

import           Data.Binary                    (Binary)
import qualified Data.Binary                    as B

import           Control.DeepSeq

import           Holumbus.Index.Common.Document

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

decompress  :: CompressedDoc -> DocumentRaw
decompress  = B.decode . BZ.decompress . unCDoc

-- | 'Document' to 'CompressedDoc' conversion.
compress    :: DocumentRaw -> CompressedDoc
compress    = CDoc . BZ.compress . B.encode

wrapDoc     :: DocumentRaw -> DocumentWrapper
wrapDoc     = newDocument' . compress

newDocument' :: CompressedDoc -> DocumentWrapper
newDocument' d = DocumentWrapper
  { _getDoc  = decompress d
  , _setDoc  = newDocument' . compress
  }