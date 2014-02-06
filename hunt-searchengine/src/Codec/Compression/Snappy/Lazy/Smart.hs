module Codec.Compression.Snappy.Lazy.Smart
where

#if  __GLASGOW_HASKELL__ >= 770
import qualified Codec.Compression.Snappy.Lazy as Zip
#endif

import qualified Data.ByteString.Lazy          as BL
import           Data.Int

import           Debug.Trace                   (trace)

-- ----------------------------------------------------------------------------
--
-- select the default compression/decompression method
--
-- compress'              is pure compression
-- compressWithTrace      traces the compression process
-- compressSmart          does not compress short ByteStrings <= 80 bytes
-- compressSmartWithTrace traces the compression process

compress :: BL.ByteString -> BL.ByteString
-- compress = compress'
-- compress = compressWithTrace
compress = compressSmart
-- compress = compressSmartWithTrace

decompress :: BL.ByteString -> BL.ByteString
-- decompress = decompress'
decompress = decompressSmart

-- ----------------------------------------------------------------------------

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

-- ----------------------------------------------------------------------------

compressWithTrace :: BL.ByteString -> BL.ByteString
compressWithTrace = traceCompress compress'

-- ----------------------------------------------------------------------------

--- XXX: is 80 a good value for snappy too?
compressSmart :: BL.ByteString -> BL.ByteString
compressSmart = compressCond 80 compress'

compressSmartWithTrace :: BL.ByteString -> BL.ByteString
compressSmartWithTrace = traceCompress compressSmart

decompressSmart :: BL.ByteString -> BL.ByteString
decompressSmart = decompressCond decompress'

{-# INLINE compressSmart #-}
{-# INLINE decompressSmart #-}

-- ----------------------------------------------------------------------------
-- XXX: code duplication (BZip.Smart) -> move to separate module

compressCond :: Int64 -> (BL.ByteString -> BL.ByteString) -> (BL.ByteString -> BL.ByteString)
compressCond lowerBound cf x
    | BL.length x <= lowerBound
        = BL.cons 0 x
    | otherwise
        = BL.cons 1 $ cf x

decompressCond :: (BL.ByteString -> BL.ByteString) -> (BL.ByteString -> BL.ByteString)
decompressCond df x
    | BL.head x == 0
        = BL.tail x
    | otherwise
        = df $ BL.tail x

{-# INLINE compressCond #-}
{-# INLINE decompressCond #-}

-- ----------------------------------------------------------------------------

traceCompress :: (BL.ByteString -> BL.ByteString) -> (BL.ByteString -> BL.ByteString)
traceCompress f x
    = trace msg y
    where
      lin  = BL.length x
      y    = f x
      lout = BL.length y
      tod  = fromInteger . fromIntegral
      cp   = (tod lout / tod lin) :: Double
      msg  = "traceCompress: " ++ show (lin, lout) ++ ", factor = " ++ show cp

-- ----------------------------------------------------------------------------
