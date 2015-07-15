{-# LANGUAGE BangPatterns #-}
module Hunt.FST(
    compileList3
  , compileListTrie
  ) where

import           Hunt.FST.Arcs (Arc)
import qualified Hunt.FST.Arcs as Arcs
import qualified Hunt.FST.Dot as Dot
import qualified Hunt.FST.Packed as Packed
import           Hunt.FST.Register
import qualified Hunt.FST.Trie as Trie
import           Hunt.FST.Types

import           Data.Text (Text)
import qualified Data.Text.Array as TextArray
import qualified Data.Text.Internal as Text
import           Data.Word

uncompile :: Text -> a -> [UncompiledState]
uncompile (Text.Text arr off len) a
  = loop 0
  where
    loop i | i < len   = toUc (TextArray.unsafeIndex arr (off+i)) : loop (i + 1)
           | otherwise = []
    toUc w
      = UncompiledState w Arcs.empty
{-# INLINE uncompile #-}

compileSuffix :: ReplaceOrRegister r a
              -> [UncompiledState]
              -> r
              -> (Arc -> r -> a)
              -> a
compileSuffix ror = go
  where
    go [] _ _ = error "compileSuffix: empty suffix"
    go (UncompiledState label arcs : sx) r k =
      case sx of
        [] -> ror (UncompiledState label arcs) r k
        _  -> go sx r
              (\arc r' ->
                ror (UncompiledState label (arc `Arcs.cons` arcs)) r' k
              )
{-# INLINE compileSuffix #-}

compile :: ReplaceOrRegister r a
        -> UncompiledState
        -> [UncompiledState]
        -> [UncompiledState]
        -> r
        -> ([UncompiledState] -> r -> a)
        -> a
compile _ _ [] (_:_) _ _  = error "compile: no lexicographic order"
compile _ prev new [] r k = k (prev:new) r
compile ror prev new@(n:nx) old@(o:ox) r k
  | ucLabel n == ucLabel o =
    compile ror o nx ox r (k . (:) prev)
  | otherwise              =
    compileSuffix ror old r (\arc ->
                              k ((prev { ucArcs = arc `Arcs.cons` ucArcs prev }):new)
                            )
{-# INLINE compile #-}

compileList :: ReplaceOrRegister r (StateRef, r) -> r -> [(Text, a)] -> (StateRef, r)
compileList ror reg ws = go ws [UncompiledState 255 Arcs.empty] reg
  where
    result rootArc r'          = (Arcs.arcTarget rootArc, r')

    go  (_:_) []   _           = error "compileList: empty path"
    go []     path r           = compileSuffix ror path r result
    go ((w, a):wx) (root:px) r = compile ror root (uncompile w a) px r (go wx)
{-# INLINE compileList #-}

--compileList1 :: [[Word8]] -> (StateRef, Packed.Register)
--compileList1 = compileList Packed.replaceOrRegister Packed.empty

--compileList2 :: [[Word8]] -> (StateRef, Trie.Register)
--compileList2 = compileList Trie.replaceOrRegister Trie.empty

compileList3 :: Register1 r => [(Text, a)] -> (StateRef, r)
compileList3 = compileList replaceOrRegister empty

--compileListTrie :: [[Word8]] -> (StateRef, Trie.Register)
--compileListTrie = compileList3

compileListTrie :: [(Text, a)] -> (StateRef, Trie.Register)
compileListTrie = compileList3


--compileListDot :: [[Word8]] -> (StateRef, Dot.Register Trie.Register)
--compileListDot = compileList3
