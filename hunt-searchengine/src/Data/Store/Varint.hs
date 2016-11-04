{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.Store.Varint where

import Data.Store
import           Data.Store.Core
import           Foreign.Ptr
import           GHC.Exts
import           GHC.Types

newtype Varint = Varint Int
               deriving (Eq, Ord, Show)

instance Store Varint where
  size = VarSize sizeVarint
  {-# INLINE size #-}
  poke vi = pokeVarint vi
  {-# INLINE poke #-}
  peek = peekVarint
  {-# INLINE peek #-}

sizeVarint :: Varint -> Int
sizeVarint (Varint (I# i)) = I# (loop 0# (int2Word# i))
  where
    loop n w =
      case n +# 1# of
        n' | isTrue# (ltWord# w 0x80##) -> n'
           | otherwise -> loop n' (uncheckedShiftRL# w 7#)
{-# INLINE sizeVarint #-}

pokeVarint :: Varint -> Poke ()
pokeVarint (Varint (I# i)) = Poke go
  where
    go targetState targetOffset =
      case pokeStatePtr targetState `plusPtr` targetOffset of
        Ptr targetPtr -> IO $ \s ->
          case loop (int2Word# i) targetPtr s of
            (# s1, targetPtr' #) ->
              case Ptr targetPtr' `minusPtr` pokeStatePtr targetState of
                targetOffset' -> (# s1, (targetOffset', ()) #)

    loop w p s0
      | isTrue# (ltWord# w 0x80##) =
          case writeWord8OffAddr# p 0# (narrow8Word# w) s0 of
            s1 -> (# s1, plusAddr# p 1# #)
      | otherwise =
          case writeWord8OffAddr# p 0# ((narrow8Word# (or# w (int2Word# 0x80#)))) s0 of
            s1 -> loop (uncheckedShiftRL# w 7#) (plusAddr# p 1#) s1
{-# INLINE pokeVarint #-}

peekVarint :: Peek Varint
peekVarint = Peek go
  where
    go _ps (Ptr sourcePtr) = IO $ \s ->
      case loop 0# 0## sourcePtr s of
        (# s1, p, w #) -> (# s1, (Ptr p, Varint (I# (word2Int# w))) #)

    loop i w p s0 =
      case readWord8OffAddr# p 0# s0 of
        (# s1, b #) -> case or# w (uncheckedShiftL# b i) of
          w' | isTrue# (word2Int# (and# 0x80## (narrow8Word# b)) /=# 0#) ->
                 loop (i +# 7#) w' (plusAddr# p 1#) s1
             | otherwise ->
                 (# s1, plusAddr# p 1#, w' #)
{-# INLINE peekVarint #-}
