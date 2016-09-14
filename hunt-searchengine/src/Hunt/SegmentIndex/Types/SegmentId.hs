{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hunt.SegmentIndex.Types.SegmentId where

import           Control.Monad.Primitive
import           Data.Primitive.PrimRef

-- | Uniquely identifies a 'Segment'.
newtype SegmentId = SegmentId { unSegmentId :: Int }
                  deriving (Eq, Ord, Show, Enum)

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
