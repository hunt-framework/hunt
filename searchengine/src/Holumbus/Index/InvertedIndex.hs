module Holumbus.Index.InvertedIndex where

import           Prelude                           as P

import           Control.Applicative               ((<$>))
import           Control.Arrow                     (first, second)

import           Data.Text                         (pack, unpack)

import           Holumbus.Data.PrefixTree          (PrefixTree)
import qualified Holumbus.Data.PrefixTree          as PT

import           Holumbus.Index.Common
import           Holumbus.Index.Common.Compression hiding (delete)
import qualified Holumbus.Index.Common.DocIdMap    as DM
import qualified Holumbus.Index.Common.Occurrences as Occ
import           Holumbus.Index.Index              as Ix
import           Holumbus.Index.Proxy.ContextIndex

------------------------------------------------------------------------
-- index implementation for simple prefixtree
-- TODO: functor/cfunctor instances

newtype DmPrefixTree v w = DmPT (PrefixTree (v w))
    deriving Show

instance Index (DmPrefixTree v) where
    type IKey (DmPrefixTree v) w = PT.Key
    type IVal (DmPrefixTree v) w = DocIdMap w
    type ICon (DmPrefixTree v) w = (v ~ DocIdMap)

    insert k v (DmPT pt)
        = DmPT $ PT.insert k v pt

    batchDelete ks (DmPT pt)
        = DmPT $ PT.map (\m -> DM.diffWithSet m ks) pt

    empty
        = DmPT $ PT.empty

    fromList
        = DmPT . PT.fromList

    toList (DmPT pt)
        = PT.toList pt

    -- TODO: use indextype parameter for real search
    search t k (DmPT pt)
        = case t of
            _ -> PT.prefixFindWithKey k pt

    unionWith op (DmPT pt1) (DmPT pt2)
        = DmPT $ PT.unionWith op pt1 pt2

    map f (DmPT pt)
        = DmPT $ PT.map f pt

-------------------------------------------------------------------------
-- setting compressed prefixtree ontop of simple prefixtree index

-- ghc is unable to infer the right kind for v
newtype ComprPrefixTree (v :: * -> *) w cv cw = ComprPT { comprPT :: DmPrefixTree cv cw}
    deriving Show

instance Index (ComprPrefixTree v w cv) where
    type IKey (ComprPrefixTree v w cv) cw = PT.Key
    type IVal (ComprPrefixTree v w cv) cw = v w
    type ICon (ComprPrefixTree v w cv) cw = (Compression (v w) (cv cw), cv ~ DocIdMap)

    insert k v (ComprPT i)
        = ComprPT $ insert k (compress v) i

    batchDelete ks (ComprPT i)
        = ComprPT $ batchDelete ks i

    empty
        = ComprPT $ empty

    fromList l
        = ComprPT . fromList $ P.map (second compress) l

    toList (ComprPT i)
        = second decompress <$> toList i

    search t k (ComprPT i)
        = second decompress <$> search t k i

    unionWith op (ComprPT i1) (ComprPT i2)
        = ComprPT $ unionWith (\o1 o2 -> compress $ op (decompress o1) (decompress o2)) i1 i2

    map f (ComprPT i)
        = ComprPT $ Ix.map (compress . f . decompress) i

-------------------------------------------------------------------------
-- setting inverted index with enforced val ~ Occurrences ontop of
-- compr prefixtree

type ContextInvertedIndex = ContextIndex InvertedIndex Occurrences

newtype InvertedIndex v
    = InvIx { invIx :: ComprPrefixTree DocIdMap Positions DocIdMap CompressedPositions }
  deriving Show

instance Compression Occurrences CompressedOccurrences where
    compress   = deflateOcc
    decompress = inflateOcc

instance Index InvertedIndex where
    type IKey InvertedIndex v = Word
    type IVal InvertedIndex v = Occurrences
    -- XXX: constraint unnecessary?
    -- type ICon InvertedIndex v = (v ~ Positions)

    insert w o
        = unionWith Occ.merge (singleton w o)

    batchDelete docIds (InvIx (ComprPT pt))
        = InvIx $ ComprPT $ Ix.map (differenceWithKeySet docIds) pt

    empty
        = InvIx $ empty

    fromList l
        = InvIx $ fromList $ P.map (first unpack) l

    toList (InvIx i)
        = first pack <$> toList i

    search t k (InvIx i)
        = first pack <$> search t (unpack k) i

    unionWith op (InvIx i1) (InvIx i2)
        = InvIx $ unionWith op i1 i2

    map f (InvIx i)
        = InvIx $ Ix.map f i

-- | Single word InvertedIndex
singleton :: Word -> Occurrences -> InvertedIndex v
singleton w o = InvIx $ (insert (unpack w) o empty)
