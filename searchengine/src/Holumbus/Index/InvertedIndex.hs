{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Holumbus.Index.InvertedIndex
( InvertedIndex(..)
)
where

import           Prelude                                         as P

import           Control.DeepSeq
import           Data.Binary                                     (Binary (..))
import           Data.Bijection.Instances                        ()

import           Holumbus.Common.BasicTypes
import           Holumbus.Common.Occurrences                     (Occurrences)
import qualified Holumbus.Common.Occurrences                     as Occ
import           Holumbus.Common.Occurrences.Compression.Snappy

import           Holumbus.Index.ComprPrefixTreeIndex
import           Holumbus.Index.Index                            as Ix

import           Holumbus.Index.Proxy.CachedIndex
import           Holumbus.Index.Proxy.CompressedIndex
import           Holumbus.Index.Proxy.KeyIndex

import           Data.Text (Text)

-- ----------------------------------------------------------------------------

newtype InvertedIndex _v
    = InvIx { invIx :: KeyProxyIndex Text ComprOccPrefixTree CompressedOccurrences }
    deriving (Eq, Show, NFData)

--mkInvIx :: CachedIndex (TextKeyProxyIndex (ComprOccIndex DmPrefixTree CompressedPositions)) Positions
--        -> InvertedIndex v_
mkInvIx x = InvIx $! x

-- ----------------------------------------------------------------------------

instance Binary (InvertedIndex v) where
    put (InvIx i) = put i
    get = get >>= return . mkInvIx

-- ----------------------------------------------------------------------------

instance Index InvertedIndex where
    type IKey InvertedIndex v = Word
    type IVal InvertedIndex v = Occurrences

    insert w o
        = unionWith Occ.merge (mkInvIx $ insert w o empty) -- InvIx $ insert w o i

    batchDelete docIds (InvIx i)
        = mkInvIx $ batchDelete docIds i

    empty
        = mkInvIx $ empty

    fromList l
        = mkInvIx $ fromList l

    toList (InvIx i)
        = toList i

    search t k (InvIx i)
        = search t k i

    lookupRange k1 k2 (InvIx i)
        = lookupRange k1 k2 i

    unionWith op (InvIx i1) (InvIx i2)
        = mkInvIx $ unionWith op i1 i2

    map f (InvIx i)
        = mkInvIx $ Ix.map f i

    keys (InvIx i)
        = keys i
