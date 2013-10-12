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
-- index implementation for simple 'DocIdMap' based 'PrefixTree'
-- TODO: functor/cfunctor instances

newtype DmPrefixTree v = DmPT (PrefixTree (DocIdMap v))
    deriving Show

instance Index DmPrefixTree where
    type IKey DmPrefixTree v = PT.Key
    type IVal DmPrefixTree v = DocIdMap v

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

    keys (DmPT pt)
        = PT.keys pt

-------------------------------------------------------------------------
-- using the PrefixTree index implementation to build an index
-- that stores Occurrences of different compressed formats

newtype ComprOccPrefixTree cv = ComprPT { comprPT :: DmPrefixTree cv}
    deriving Show

instance Index ComprOccPrefixTree where
    type IKey ComprOccPrefixTree v = PT.Key
    type IVal ComprOccPrefixTree v = Occurrences
    type ICon ComprOccPrefixTree v = (OccCompression (DocIdMap v))

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

    keys (ComprPT i)
        = keys i

-------------------------------------------------------------------------
-- using the ComprOccPrefixTree to build an inverted index implementation.
-- Also using the generic ContextIndex implmenetation to add contexts to
-- the inverted index without having to change the implementation itself

type ContextInvertedIndex = ContextIndex InvertedIndex Occurrences

newtype InvertedIndex v
    = InvIx { invIx :: ComprOccPrefixTree CompressedPositions }
    deriving Show

instance Index InvertedIndex where
    type IKey InvertedIndex v = Word
    type IVal InvertedIndex v = Occurrences

    insert w o
        = unionWith Occ.merge (InvIx $ insert (unpack w) o empty)

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

    keys (InvIx i)
        = P.map pack $ keys i -- XXX: pack unnecessary, maybe change IKeys
