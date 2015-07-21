{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Hunt.FST.Trie where

-- | This modules defines a replaceOrRegister function which creates a
--   packed trie. The trie can be constructed without any intermediate
--   data structures.

import           Hunt.FST.Arcs (Arc(..), Arcs)
import qualified Hunt.FST.Arcs as Arcs
import           Hunt.FST.Register

import           Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Internal.Write as Blaze
import           Control.DeepSeq
import           Data.ByteString (ByteString)
import           Data.Monoid
import           Data.Word
import qualified Data.ByteString.Base16 as Base16

data Trie
  = Trie { trBytes :: !ByteString
         } deriving (Eq)

instance NFData Trie where
  rnf tr
    = trBytes tr `seq` ()

instance Show Trie where
  show (Trie t) = show (Base16.encode t)

type Offset = Word64

data Register = Register {
    regBuffer   :: !Builder -- ^ Buffer for compiled trie
  , regSize     :: !Int     -- ^ Current size of the compiled trie
  , regNextOut  :: !Int     -- ^ Index where to stick next output
  }

instance Register1 Register where
  type Output Register = Trie
  empty             = Hunt.FST.Trie.empty
  output            = Hunt.FST.Trie.output
  replaceOrRegister = Hunt.FST.Trie.replaceOrRegister


empty :: Register
empty
  = Register mempty 0 0

output :: Register -> Trie
output = Trie . Blaze.toByteString . regBuffer
{-# INLINE output #-}

-- | Compiles an `UncompiledState` into an `Arc` which
--   points to the transitions in that that (compiled) state.
replaceOrRegister :: UncompiledState
                  -> Register
                  -> (Arc -> Register -> a)
                  -> a
replaceOrRegister (UncompiledState label arcs) reg k
  = k (Arc label 0 (fromIntegral (regSize reg'))) reg' -- directly using regSize reg' is 15% faster than using size' variable
  where
    reg'
      = reg { regBuffer   = buffer'
            , regSize     = size'
            , regNextOut  = nextOut'
            }
    nextOut'
      = regNextOut reg
    size'
      = regSize reg + nBytes
    buffer'
      = bytes `mappend` regBuffer reg
    (nBytes, bytes)
      = compileArcs (regSize reg) arcs
{-# INLINE replaceOrRegister #-}

-- | Compiles a set of `Arc`s to efficient byte representation.
--   `compileArcs` tries to find the most optimized representation
--   for given `Arcs`, where base is the offset these bytes are being placed.
compileArcs :: Int -> Arcs -> (Int, Builder)
compileArcs base arcs
  = (Blaze.getBound bytes, Blaze.fromWrite bytes)
  where
    bytes
      = case Arcs.length arcs of
         1 -> compileSingleArc base (Arcs.head arcs)
         _ -> compileMultipleArcs base arcs
{-# INLINE compileArcs #-}

-- | Compiled multiple `Arc`s to byte code.
compileMultipleArcs :: Int -> Arcs -> Blaze.Write
compileMultipleArcs _base arcs
  = Blaze.writeWord8 1
    `mappend` Blaze.writeWord16be (fromIntegral (Arcs.length arcs))
    `mappend` go (Arcs.arcs arcs)
  where
    go :: [Arc] -> Blaze.Write
    go []
      = mempty
    go (Arc label _ target : ax)
      = go ax
        `mappend` Blaze.writeWord16be label
        `mappend` Blaze.writeWord64be target
{-# INLINE compileMultipleArcs #-}

-- | Compiles a single `Arc` to byte code.
compileSingleArc :: Int -> Arc -> Blaze.Write
compileSingleArc base arc
  | isPrevious = compileSingleNextArc arc
  | otherwise  = compileSingleArc' base arc
  where
    isPrevious
      = fromIntegral base == arcTarget arc
{-# INLINE compileSingleArc #-}

-- | Compiles a single `Arc` to byte code.
--   We know that the previous written `Arc`
--   is the only successor of the `Arc` being
--   compiled.
compileSingleNextArc :: Arc -> Blaze.Write
compileSingleNextArc arc
  = Blaze.writeWord8 flags
    `mappend` Blaze.writeWord16be (arcLabel arc)
  where
    flags = 0x02
{-# INLINE compileSingleNextArc #-}

-- | Compile an where we know that it only points
--   to one target.
compileSingleArc' :: Int -> Arc -> Blaze.Write
compileSingleArc' _base arc
  = Blaze.writeWord8 flags
    `mappend` Blaze.writeWord16be (arcLabel arc)
    `mappend` Blaze.writeWord64be (arcTarget arc)
  where
    flags = 0x04 :: Word8
{-# INLINE compileSingleArc'#-}
