-- ----------------------------------------------------------------------------
{- |
  Snappy 'ByteString' compression that is only used if the result is sufficiently large
  (80 characters).

  This is based on observation for the BZIP compression where small compressed 'ByteString's
  tend to be bigger than the source 'ByteString'.
-}
-- ----------------------------------------------------------------------------

module Codec.Compression.BZip.Smart
  ( compress, decompress
  , compressSmartWithTrace
  )
where

import           Codec.Compression.Utility
import qualified Codec.Compression.BZip        as Zip

import qualified Data.ByteString.Lazy          as BL

-- ------------------------------------------------------------
--
-- select the default compression/decompression method
--
-- compress'                is pure compression
-- traceCompress compress'  additionally traces the compression process
-- compressSmart            does not compress short ByteStrings <= 80 bytes
-- compressSmartWithTrace   additionally traces the compression process

-- | Compress a data stream with bzip (if it is bigger than 80 characters).
compress :: BL.ByteString -> BL.ByteString
-- compress = compress'
-- compress = traceCompress compress'
compress = compressSmart
-- compress = compressSmartWithTrace

-- | Decompress a data stream.
decompress :: BL.ByteString -> BL.ByteString
-- decompress = decompress'
decompress = decompressSmart

-- ------------------------------------------------------------

-- | Plain compression.
compress'   :: BL.ByteString -> BL.ByteString
compress'   = Zip.compress

-- | Plain decompression.
decompress' :: BL.ByteString -> BL.ByteString
decompress' = Zip.decompress

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
