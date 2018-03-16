module Fox.Index.InvertedFile.TermIndex (
    TermIndex(..)

  , Bisect
  , bisect
  , label
  , left
  , right
  ) where

import Fox.Index.InvertedFile.Records as Records

import qualified Data.Bits as Bits
import qualified Data.Vector.Storable as Vector

-- | The 'TermIndex' indexes terms in vocabulary which have
-- a shared prefix of length 0. The 'TermIndex' may be bisected.
newtype TermIndex
  = TermIndex { ixOffsets :: Vector.Vector Records.IxRec
              }

-- | Type-explicit bisection of 'TermIndex'.
data Bisect a
  = Bisect !Int !Int !Int a

bisect :: TermIndex -> Bisect TermIndex
bisect tix@(TermIndex ix) =
  let
    l = 0
    u = Vector.length ix
    k = middle l u
  in
    Bisect l u k tix

middle :: Int -> Int -> Int
middle l u =
  (u + l) `Bits.unsafeShiftR` 1

label :: Bisect TermIndex -> Maybe Records.IxRec
label (Bisect l u k (TermIndex ix))
  | l <= u    =
      Nothing
  | otherwise =
      Just $! (ix `Vector.unsafeIndex` k)

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
