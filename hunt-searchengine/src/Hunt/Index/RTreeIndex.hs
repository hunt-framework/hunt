{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

-- ----------------------------------------------------------------------------
{- |
  Text index using the 'DocIdMap' based on the 'StringMap' implementation.
-}
-- ----------------------------------------------------------------------------

module Hunt.Index.RTreeIndex
(   RTreeIndex(..)
  , SimpleRTreeIndex(..)
  , readPosition
  , showPosition
)
where

import           Control.DeepSeq

import           Data.Binary                          (Binary (..))
import qualified Data.List                            as L
import           Data.Monoid                          ((<>))
import qualified Data.RTree.Strict                    as RT
import           Data.RTree.MBB
import           Data.Text                            (Text)
import qualified Data.Text                            as T (pack, unpack)
import           Data.Typeable
import           Data.Bijection

import           Hunt.Index
import qualified Hunt.Index                           as Ix
import           Hunt.Index.Proxy.KeyIndex
import           Hunt.Common.IntermediateValue
import           Hunt.Common.DocIdSet                 (DocIdSet)
import           Hunt.Common.Occurrences              (Occurrences)
import           Hunt.Index.Schema.Normalize.Position (position)

import           Text.Parsec


-- ------------------------------------------------------------

-- | Index adapter for 'Data.RTree' data structure

newtype RTreeIndex v
  = DmRT { dmRT :: RT.RTree v }
  deriving (Eq, Show, NFData, Typeable)

mkDmRT :: RT.RTree v -> RTreeIndex v
mkDmRT v = DmRT $! v

-- ------------------------------------------------------------

instance IndexValue x => Binary (RTreeIndex x) where
  put = put . dmRT
  get = get >>= return . mkDmRT

-- ------------------------------------------------------------

instance Index (RTreeIndex v) where
  type IKey (RTreeIndex v) = RT.MBB
  type IVal (RTreeIndex v) = v

  insertList kvs (DmRT rt) =
    mkDmRT $ L.foldl' (\ m' (k', v') -> RT.insertWith mergeValues k' v' m') rt (fromIntermediates kvs)

    {- same problem as in PrefixTreeIndex, the k' in kvs don't need to be unique

       mkDmRT $ RT.unionWith op rt (RT.fromList kvs)
    -}

  deleteDocs ks (DmRT rt)
    = mkDmRT $ RT.mapMaybe (diffValues ks) rt

  empty
    = mkDmRT $ RT.empty

  fromList
    = mkDmRT . RT.fromList . fromIntermediates

  toList (DmRT rt)
    = toIntermediates . RT.toList $ rt

  -- MBBs don't have any case or prefix
  search _ k (DmRT rt)
    = toIntermediates $ RT.lookupRangeWithKey k rt

  lookupRange k1 k2 (DmRT rt)
    = toIntermediates $ RT.lookupRangeWithKey (unionMBB k1 k2) rt

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

readPosition :: Text -> MBB
readPosition t
  = case parse position "" $ T.unpack t of
     Right (la,lo) -> mbb la lo la lo
     Left _        -> error "readPosition positon: invalid"

showPosition :: MBB -> Text
showPosition (MBB la lo _ _ ) = T.pack (show la) <> "-" <> T.pack (show lo)

-- ------------------------------------------------------------
-- Index using RTree for indexing positions and bounding boxes
-- ------------------------------------------------------------

-- | Newtype to allow date normalization 'Bijection' instance.

instance Bijection MBB Text where
  to   = showPosition
  from = readPosition

-- ------------------------------------------------------------

-- | Date index using a 'StringMap'-implementation.
newtype SimpleRTreeIndex
  = InvRTreeIx { invRTreeIx :: KeyProxyIndex Text (RTreeIndex DocIdSet)}
  deriving (Eq, Show, NFData, Typeable)

mkInvRTreeIx :: KeyProxyIndex Text (RTreeIndex DocIdSet) -> SimpleRTreeIndex
mkInvRTreeIx x = InvRTreeIx $! x

-- ------------------------------------------------------------

instance Binary SimpleRTreeIndex where
  put = put . invRTreeIx
  get = get >>= return . mkInvRTreeIx

-- ------------------------------------------------------------

instance Index SimpleRTreeIndex where
  type IKey SimpleRTreeIndex = Text
  type IVal SimpleRTreeIndex = DocIdSet

  insertList wos (InvRTreeIx i)
    = mkInvRTreeIx $ insertList wos i

  deleteDocs docIds (InvRTreeIx i)
    = mkInvRTreeIx $ deleteDocs docIds i

  empty
    = mkInvRTreeIx $ empty

  fromList l
    = mkInvRTreeIx $ fromList l

  toList (InvRTreeIx i)
    = toList i

  search t k (InvRTreeIx i)
    = search t k i

  lookupRange k1 k2 (InvRTreeIx i)
    = lookupRange k1 k2 i

  unionWith op (InvRTreeIx i1) (InvRTreeIx i2)
    = mkInvRTreeIx $ unionWith op i1 i2

  map f (InvRTreeIx i)
    = mkInvRTreeIx $ Ix.map f i

  mapMaybe f (InvRTreeIx i)
    = mkInvRTreeIx $ Ix.mapMaybe f i

  keys (InvRTreeIx i)
    = keys i


