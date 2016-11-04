{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hunt.SegmentIndex.Types.SegmentId where

import           Control.Monad.Primitive
import           Data.Binary             (Binary)
import           Data.Char
import           Data.Primitive.PrimRef
import           Data.Store
import           Numeric                 (readInt, showIntAtBase)

-- | Uniquely identifies a 'Segment'.
newtype SegmentId = SegmentId { unSegmentId :: Int }
                  deriving (Store, Binary, Eq, Ord, Enum)

segmentZero :: SegmentId
segmentZero = SegmentId 1

instance Show SegmentId where
  show (SegmentId sid) = '_' : showIntAtBase 36 intToDigit36 sid ""
    where intToDigit36 n | n < 10    = chr (n + ord '0')
                         | otherwise = chr (n + ord 'a' - 10)

instance Read SegmentId where
  readsPrec _ ('_':s) =
    case readInt 36 isAsciiAlphaNum digitToInt36 s of
      [(sid, "")] -> [(SegmentId sid, "")]
      _           -> []
    where
      isAsciiAlphaNum :: Char -> Bool
      isAsciiAlphaNum c = isAscii c && (isDigit c || isLetter c)

      digitToInt36 :: Char -> Int
      digitToInt36 c
        | isDigit c            =  ord c - ord '0'
        | c >= 'a' && c <= 'z' =  ord c - ord 'a' + 10
        | c >= 'A' && c <= 'Z' =  ord c - ord 'A' + 10
        | otherwise            =
            error ("Base36.digitToInt: not a digit " ++ show c) -- sigh
  readsPrec _ _ = []

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
