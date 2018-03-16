{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE UnboxedTuples     #-}
module Fox.IO.Read (
    BufferRange(..)
  , Read(..)

  , UTF16(..)
  , utf16

  , varword
  , varint
  , word8
  , word64
  ) where

import qualified GHC.Exts as GHC
import qualified GHC.Types as GHC

import qualified Foreign.Ptr as Foreign
import qualified Foreign.Storable as Storable
import qualified Data.Word as Word
import Prelude hiding (Read)

data UTF16
  = UTF16 !(Foreign.Ptr Word.Word8) !Int

data BufferRange
  = BufferRange !(Foreign.Ptr Word.Word8)
                !(Foreign.Ptr Word.Word8)

newtype Read a
  = R { runRead :: Foreign.Ptr Word.Word8
                -> Foreign.Ptr Word.Word8
                -> IO (a, Foreign.Ptr Word.Word8) }

instance Functor Read where
  fmap = mapRead

instance Applicative Read where
  pure  = pureRead
  (<*>) = apRead

instance Monad Read where
  (>>=) = bindRead

pureRead :: a -> Read a
pureRead x = R (\_ op -> pure (x, op))

apRead :: Read (a -> b) -> Read a -> Read b
apRead (R f) (R g) = R (\r op -> do (f', op') <- f r op
                                    (g', op'') <- g r op'
                                    return (f' g', op''))

bindRead :: Read a -> (a -> Read b) -> Read b
bindRead (R m) f = R (\r op -> do (a, op') <- m r op
                                  runRead (f a) r op')

mapRead :: (a -> b) -> Read a -> Read b
mapRead f (R g) =
  R (\range op -> do (x, y) <- g range op
                     return (f x, y))

readStorable :: Storable.Storable a => Read a
readStorable = R f
  where
    f _ op = do
      a <- Storable.peek (Foreign.castPtr op)
      return (a, op `Foreign.plusPtr` Storable.sizeOf a)

word8 :: Read Word.Word8
word8 = readStorable

word64 :: Read Word.Word64
word64 = readStorable

utf16 :: Read UTF16
utf16 = R go
  where
    R readVarint = varint

    go r op = do
      (l, op') <- readVarint r op
      return (UTF16 op' l, op' `Foreign.plusPtr` l)

varword :: Read Word.Word
varword = R go
  where
    go _ (GHC.Ptr op) = GHC.IO $ \s0 ->
      case loop 0## 1## op s0 of
        (# s1, (# a, op' #) #) ->
          (# s1, (GHC.W# a, GHC.Ptr op') #)

    loop :: GHC.Word#
         -> GHC.Word#
         -> GHC.Addr#
         -> GHC.State# GHC.RealWorld
         -> (# GHC.State# GHC.RealWorld, (# GHC.Word#, GHC.Addr# #) #)
    loop acc shift op s0 =
      case GHC.readWord8OffAddr# op 0# s0 of
        (# s1, w #) ->
          let
            acc' =
              GHC.plusWord# acc (GHC.timesWord# (GHC.and# w 127##) shift)
            op'  =
              GHC.plusAddr# op 1#
            shift' =
              GHC.uncheckedShiftL# shift 7#
          in
            if GHC.isTrue# (GHC.ltWord# w (GHC.int2Word# 0x80#))
            then
              (# s1, (# acc', op' #) #)
            else
              loop (GHC.plusWord# acc' shift') shift' op' s1

varint :: Read Int
varint = fmap fromIntegral varword
