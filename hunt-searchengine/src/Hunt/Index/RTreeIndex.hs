{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

-- ----------------------------------------------------------------------------
{- |
  Text index using the 'DocIdMap' based on the 'StringMap' implementation.
-}
-- ----------------------------------------------------------------------------

module Hunt.Index.RTreeIndex
( DmRTree(..)
)
where

import           Control.DeepSeq

import           Data.Binary                (Binary (..))
import           Data.Typeable
import qualified Data.Maybe    as Maybe (mapMaybe)

import qualified Data.RTree      as RT
import           Data.RTree.MBB

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocIdMap       as DM
import           Hunt.Index

import           Hunt.Utility

-- ------------------------------------------------------------

-- | Index using 'Data.RTree'
newtype DmRTree v
  = DmRT { dmRT :: RT.RTree (DocIdMap v) }
  deriving (Eq, Show, NFData, Typeable)

mkDmRT :: NFData v => RT.RTree (DocIdMap v) -> DmRTree v
mkDmRT v = DmRT $! v

-- ------------------------------------------------------------

instance (NFData v,Binary v) => Binary (DmRTree v) where
  put = put . dmRT
  get = get >>= return . mkDmRT

-- ------------------------------------------------------------

instance Index DmRTree where
  type IKey DmRTree v = RT.MBB
  type IVal DmRTree v = DocIdMap v

  insertList kvs (DmRT rt) =
    mkDmRT $ RT.union rt (RT.fromList kvs)

  deleteDocs ks (DmRT rt)
    = mkDmRT $ RT.mapMaybe (\m -> let dm = DM.diffWithSet m ks
                                  in if DM.null dm then Nothing else Just dm) rt

  empty
    = mkDmRT $ RT.empty

  fromList
    = mkDmRT . RT.fromList

  toList (DmRT rt)
    = RT.toList rt

  -- MBBs don't have any case or prefix
  search _ k (DmRT rt)
    = RT.lookupRangeWithKey k rt

  lookupRange k1 k2 (DmRT rt)
    = RT.lookupRangeWithKey (unionMBB k1 k2) rt

  unionWith op (DmRT rt1) (DmRT rt2)
    = mkDmRT $ RT.unionWith op rt1 rt2


  map f (DmRT rt)
    = mkDmRT $ fmap f rt

  -- prohibitiv expensive.
  mapMaybe f (DmRT rt)
    = mkDmRT $ RT.mapMaybe f rt

  keys (DmRT rt)
    = RT.keys rt

-- ------------------------------------------------------------
