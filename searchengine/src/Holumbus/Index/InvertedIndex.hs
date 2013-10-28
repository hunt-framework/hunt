module Holumbus.Index.InvertedIndex
( InvertedIndex(..)
)
where

import           Prelude                             as P

import           Control.Applicative                 ((<$>))
import           Control.Arrow                       (first)

import           Data.Text                           (pack, unpack)

import           Holumbus.Common.Compression         hiding (delete)
import           Holumbus.Index.Common
--import           Holumbus.Common.Occurrences       (Occurrences)
import qualified Holumbus.Common.Occurrences         as Occ

import           Holumbus.Index.ComprPrefixTreeIndex
import           Holumbus.Index.Index                as Ix

-- ----------------------------------------------------------------------------

newtype InvertedIndex _v
    = InvIx { invIx :: ComprOccPrefixTree CompressedPositions }
    deriving Show

-- ----------------------------------------------------------------------------

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
