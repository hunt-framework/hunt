{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Hunt.FST.Trie where

import           Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Internal.Write as Blaze
import           Control.DeepSeq
import           Data.Bits
import           Data.ByteString (ByteString)
import           Hunt.FST.Arcs (Arc(..), Arcs)
import qualified Hunt.FST.Arcs as Arcs
import           Hunt.FST.Register
import           Hunt.FST.Types
import qualified Data.List as List
import           Data.Monoid
import           Data.Word

newtype Trie = Trie { unTrie :: ByteString }
               deriving (Eq, Show, NFData)

data Register = Register {
    regOffset :: !Offset
  , regWeight :: !Weight
  , regBuffer :: !Builder
  }

instance Register1 Register where
  type Output Register = Trie
  empty             = Hunt.FST.Trie.empty
  output            = Hunt.FST.Trie.output
  replaceOrRegister = Hunt.FST.Trie.replaceOrRegister

type Offset = Word64

empty :: Register
empty = Register 0 1 mempty

output :: Register -> Trie
output = Trie . Blaze.toByteString . regBuffer
{-# INLINE output #-}

isFinalArc :: Arc -> Bool
isFinalArc = (Arcs.final ==)
{-# INLINE isFinalArc #-}

replaceOrRegister :: UncompiledState -> Register -> (Arc -> Register -> a) -> a
replaceOrRegister (UncompiledState label arcs) register k =
  k (Arc label 0 (regOffset register')) register'
  where
    register' = register {
        regOffset = offset'
      , regWeight = regWeight register + if label == 0 then 1 else 0
      , regBuffer = buffer'
      }

    -- accidentally quadric
    buffer''   = Blaze.toByteString (regBuffer register)

    offset'    = (regOffset register) + fromIntegral n
    buffer'    = bytes `mappend` Blaze.fromByteString buffer''
    (n, bytes) = case label of
      0 -> compileFinalArc (regWeight register)
      _ -> compileArcs (regOffset register) arcs
{-# INLINE replaceOrRegister #-}

compileFinalArc :: Weight -> (Int, Builder)
compileFinalArc weight = (Blaze.getBound bytes, Blaze.fromWrite bytes)
 where
   bytes =  Blaze.writeWord8 0xE0
           `mappend` Blaze.writeWord32le weight
{-# INLINE compileFinalArc #-}

compileArcs :: Offset -> Arcs -> (Int, Builder)
compileArcs offset arcs = (Blaze.getBound bytes, Blaze.fromWrite bytes)
  where
    bytes = case Arcs.length arcs of
      1 -> compileSimpleArc offset (List.head (Arcs.arcs arcs))
      _ -> compileMultipleArcs offset arcs
{-# INLINE compileArcs #-}

compileMultipleArcs :: Offset -> Arcs -> Blaze.Write
compileMultipleArcs offset arcs =
  Blaze.writeWord8 0
  `mappend` Blaze.writeWord32le (fromIntegral $ Arcs.length arcs)
  `mappend` go (Arcs.arcs arcs)
  where
    go :: [Arc] -> Blaze.Write
    go []     = mempty
    go (Arc label _ target : ax) =
      go ax
      `mappend` Blaze.writeWord16le label
      `mappend` Blaze.writeWord64le (offset - target)
{-# INLINE compileMultipleArcs #-}

compileSimpleArc :: Offset -> Arc -> Blaze.Write
compileSimpleArc offset arc | isPrevious = compileSingleNextArc arc
                            | otherwise  = compileSingleArc offset arc
  where
    isPrevious = offset - arcTarget arc == 0
{-# INLINE compileSimpleArc #-}

compileSingleArc :: Offset -> Arc -> Blaze.Write
compileSingleArc offset (Arc label _ target) =
  Blaze.writeWord16le word `mappend` Blaze.writeWord64le (offset - target)
  where
    word :: Word16
    word = 0x8000 .|. (fromIntegral label .&. 0x00FF)
{-# INLINE compileSingleArc #-}

compileSingleNextArc :: Arc -> Blaze.Write
compileSingleNextArc (Arc label _ _) = Blaze.writeWord16le word
  where
    word :: Word16
    word = 0xC000 .|. (fromIntegral label .&. 0x00FF)
{-# INLINE compileSingleNextArc #-}
