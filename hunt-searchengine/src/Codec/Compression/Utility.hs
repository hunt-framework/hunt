{-# LANGUAGE CPP #-}

{- |
  Utility function for ByteString-based compression.

  This includes tracing functions as well as compression depending on the size of the input.
-}

module Codec.Compression.Utility
where

import qualified Data.ByteString.Lazy as BL
import           Data.Int

import           Debug.Trace          (trace)

-- ----------------------------------------------------------------------------

-- | Smart compression depending on the size of the 'ByteString'.
--   'decompressCond' has to be used for decompression.
compressCond :: Int64 -> (BL.ByteString -> BL.ByteString) -> (BL.ByteString -> BL.ByteString)
compressCond lowerBound cf x
    | BL.length x <= lowerBound
        = BL.cons 0 x
    | otherwise
        = BL.cons 1 $ cf x

-- | Decompress a 'ByteString' created with 'compressCond'.
decompressCond :: (BL.ByteString -> BL.ByteString) -> (BL.ByteString -> BL.ByteString)
decompressCond df x
    | BL.head x == 0
        = BL.tail x
    | otherwise
        = df $ BL.tail x

-- | Trace the compression.
--   Includes the length of the of the source and result and the ratio.
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

{-# INLINE compressCond #-}
{-# INLINE decompressCond #-}
