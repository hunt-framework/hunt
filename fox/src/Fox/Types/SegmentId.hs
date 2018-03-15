{-# LANGUAGE DeriveGeneric #-}
module Fox.Types.SegmentId where

import           Data.Char              (chr, ord)
import           Data.Primitive.PrimRef
import           Numeric                (showIntAtBase)

import GHC.Generics (Generic)

newtype SegmentId = SegmentId { unSegmentId :: Int }
                  deriving (Eq, Ord, Show, Generic)

genesis :: SegmentId
genesis = SegmentId 0

nextSegmentId :: SegmentId -> SegmentId
nextSegmentId (SegmentId i) = SegmentId (i + 1)

segmentIdToBase36 :: SegmentId -> String
segmentIdToBase36 (SegmentId segmentId) =
  showIntAtBase 36 intToDigit segmentId ""
  where
    intToDigit n
      | n < 10    = chr (n + ord '0')
      | otherwise = chr (n + ord 'a' - 10)

-- | Used for generation of new 'SegmentId's in the 'IO' monad.
newtype SegIdGen = SegIdGen (PrimRef (PrimState IO) Int)

-- | Creates a new 'SegIdGen' with first 'SegmentId' 1.
newSegIdGen :: IO SegIdGen
newSegIdGen = newSegIdGen' (SegmentId 1)

-- | Create a new 'SegIdGen' with a given start 'SegmentId'
newSegIdGen' :: SegmentId -> IO SegIdGen
newSegIdGen' (SegmentId sid) = SegIdGen <$> newPrimRef sid

-- | Generate a new 'SegmentId'. This function *is* thread-safe.
genSegId :: SegIdGen -> IO SegmentId
genSegId (SegIdGen ref) = SegmentId <$> fetchAddInt ref 1
