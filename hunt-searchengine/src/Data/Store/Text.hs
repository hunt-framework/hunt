module Data.Store.Text where

import           Data.Bits
import           Data.Primitive.ByteArray
import           Data.Store.Core
import           Data.Store.Varint
import           Data.Text.Array
import           Data.Text.Internal       (Text (..), text)
import Data.Store

newtype RawText = RawText Text
                deriving (Eq, Ord, Show)

instance Store RawText where
  size = sizeRawText
  {-# INLINE size #-}
  poke a = pokeRawText a
  {-# INLINE poke #-}
  peek = peekRawText
  {-# INLINE peek #-}

sizeRawText :: Size RawText
sizeRawText = undefined
{-# INLINE sizeRawText #-}

pokeRawText :: RawText -> Poke ()
pokeRawText (RawText (Text (Array ba) off len)) =
  case 2 * len of
    effectiveLen -> do
      _ <- pokeVarint (Varint effectiveLen)
      pokeFromByteArray ba off effectiveLen
      return ()
{-# INLINE pokeRawText #-}

peekRawText :: Peek RawText
peekRawText = do
  Varint len      <- peekVarint
  ByteArray bytes <- peekToByteArray "peekRawText" len
  return
    $ RawText
    $ text (Array bytes) 0 (len `unsafeShiftR` 1)
{-# INLINE peekRawText #-}
