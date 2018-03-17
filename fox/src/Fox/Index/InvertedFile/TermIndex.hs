module Fox.Index.InvertedFile.TermIndex (
    TermIndex(..)

  , Bisect
  , bisect
  , label
  , left
  , right
  ) where

import qualified Fox.IO.Read as Read
import qualified Fox.Index.InvertedFile.Records as Records

import qualified Data.Offset as Offset
import qualified Data.Bits as Bits
import qualified Data.Count as Count
import qualified Data.Vector.Storable as Vector

-- | The 'TermIndex' indexes terms in vocabulary which have
-- a shared prefix of length 0. The 'TermIndex' may be bisected.
data TermIndex
  = TermIndex { ixTermCount :: !(Count.CountOf (Records.VocRec Read.UTF16))
              , ixOffsets   :: !(Vector.Vector Records.IxRec)
              } deriving (Show)

-- | Type-explicit bisection of 'TermIndex'.
data Bisect a
  = Bisect !Int !Int !Int a
  deriving (Show)

bisect :: TermIndex -> Bisect TermIndex
bisect tix@(TermIndex _ ix) =
  let
    l = 0
    u = Vector.length ix
    k = middle l u
  in
    Bisect l u k tix

middle :: Int -> Int -> Int
middle l u =
  (u + l) `Bits.unsafeShiftR` 1

label
  :: Bisect TermIndex
  -> Maybe ( Offset.OffsetOf (Records.VocRec Read.UTF16)
           , Count.CountOf (Records.VocRec Read.UTF16)
           )
label (Bisect l u k (TermIndex c ix))
  | u <= l =
      Nothing
  | otherwise =
      let
        ixRec =
          ix `Vector.unsafeIndex` k

        x' =
          if k + 1 < Vector.length ix
          then
            Records.ixPrecedingTermCount
              (ix `Vector.unsafeIndex` (k + 1))
          else
            c

      in Just $! ( Records.ixVocOffset ixRec
                 , Count.diff x' (Records.ixPrecedingTermCount ixRec)
                 )

left :: Bisect TermIndex -> Bisect TermIndex
left (Bisect l _ k tix) =
  let
    l' = l
    u' = k
    k' = middle l' u'
  in
    Bisect l' u' k' tix

right :: Bisect TermIndex -> Bisect TermIndex
right (Bisect _ u k tix) =
  let
    l' = k + 1
    u' = u
    k' = middle l' u'
  in
    Bisect l' u' k' tix
