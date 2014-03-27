{-# LANGUAGE CPP #-}

-- ----------------------------------------------------------------------------
{- |
  'ByteString' compression that is only used if the result is sufficiently large
  (80 characters).

  This is based on observation for the BZIP compression where small compressed 'ByteString's
  tend to be bigger than the source 'ByteString'.
  This may not apply to the Snappy compression.
-}
-- ----------------------------------------------------------------------------

module Codec.Compression.Snappy.Lazy.Smart
where

import           Codec.Compression.Utility
#if  __GLASGOW_HASKELL__ >= 770
import qualified Codec.Compression.Snappy.Lazy as Zip
#endif

import qualified Data.ByteString.Lazy          as BL

-- TODO: This is based on bzip - test if this is also the case with snappy.

-- ------------------------------------------------------------
--
-- select the default compression/decompression method
--
-- compress'                is pure compression
-- traceCompress compress'  additionally traces the compression process
-- compressSmart            does not compress short ByteStrings <= 80 bytes
-- compressSmartWithTrace   additionally traces the compression process

-- | Compress a data stream with Snappy (if it is bigger than 80 characters).
compress :: BL.ByteString -> BL.ByteString
-- compress = compress'
-- compress = traceCompress compress'
compress = compressSmart
-- compress = compressSmartWithTrace

-- | Compress a data stream.
decompress :: BL.ByteString -> BL.ByteString
-- decompress = decompress'
decompress = decompressSmart

-- ------------------------------------------------------------

#if  __GLASGOW_HASKELL__ >= 770
compress'   :: BL.ByteString -> BL.ByteString
compress'   = Zip.compress

decompress' :: BL.ByteString -> BL.ByteString
decompress' = Zip.decompress

#else
#warning snappy is disabled if GHC < 7.7
compress'   :: BL.ByteString -> BL.ByteString
compress'   = id

decompress' :: BL.ByteString -> BL.ByteString
decompress' = id
#endif

-- ------------------------------------------------------------

-- | Smart compression with stdout tracing.
compressSmart :: BL.ByteString -> BL.ByteString
compressSmart = compressCond 80 compress'

-- | Smart compression with stdout tracing.
--   Includes the length of both the source and the result and the ratio.
compressSmartWithTrace :: BL.ByteString -> BL.ByteString
compressSmartWithTrace = traceCompress compressSmart

-- | Smart decompression.
decompressSmart :: BL.ByteString -> BL.ByteString
decompressSmart = decompressCond decompress'

{-# INLINE compressSmart #-}
{-# INLINE decompressSmart #-}

-- ------------------------------------------------------------
