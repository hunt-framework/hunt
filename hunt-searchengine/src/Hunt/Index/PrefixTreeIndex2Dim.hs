{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

-- ----------------------------------------------------------------------------
{- |
  Text index using the 'DocIdMap' based on the 'StringMap' implementation.
-}
-- ----------------------------------------------------------------------------

module Hunt.Index.PrefixTreeIndex2Dim
( DmPrefixTree(..)
)
where

import           Control.DeepSeq

import           Data.Binary               (Binary (..))
import           Data.Typeable

import qualified Data.StringMap.Dim2Search as SM2
import qualified Data.StringMap.Strict     as SM
import           Data.Bijection
import           Data.Bijection.Instances  ()
import           Data.Text                 (Text)

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocIdSet
import           Hunt.Common.IntermediateValue
import           Hunt.Index
import qualified Hunt.Index                as Ix
import           Hunt.Index.Proxy.KeyIndex

import qualified Hunt.Index.Schema.Normalize.Position as Pos

import           Hunt.Utility

-- ------------------------------------------------------------

-- | Text index using 'DocIdMap' based on the 'StringMap' implementation.
--   Note that the value parameter is on the type of the 'DocIdMap' value and not the 'Occurrences'
--   itself.
newtype DmPrefixTree v
  = DmPT { dmPT :: SM.StringMap v }
  deriving (Eq, Show, NFData, Typeable)

mkDmPT :: NFData v => SM.StringMap v -> DmPrefixTree v
mkDmPT v = DmPT $! v

-- ------------------------------------------------------------

instance IndexValue v => Binary (DmPrefixTree v) where
  put = put . dmPT
  get = get >>= return . mkDmPT

-- ------------------------------------------------------------

instance Index (DmPrefixTree v) where
  type IKey (DmPrefixTree v) = SM.Key
  type IVal (DmPrefixTree v) = v

  insertList kvs (DmPT pt) =
    mkDmPT $ SM.unionWith mergeValues pt (SM.fromList . fromIntermediates $ kvs)

  deleteDocs ks (DmPT pt)
    = mkDmPT $ SM.mapMaybe (diffValues ks) pt

  empty
    = mkDmPT $ SM.empty

  fromList
    = mkDmPT . SM.fromList . fromIntermediates

  toList (DmPT pt)
    = toIntermediates . SM.toList $ pt

  search t k (DmPT pt)
    = toIntermediates $ case t of
        Case         -> case SM.lookup k pt of
                          Nothing -> []
                          Just xs -> [(k,xs)]
        NoCase       -> luCase k pt
        PrefixCase   -> pfCase k pt
        PrefixNoCase -> pfNoCase k pt
    where
    toL      = SM.toListShortestFirst
    luCase   = toL .:: SM.lookupNoCase
    pfCase   = toL .:: SM.prefixFilter
    pfNoCase = toL .:: SM.prefixFilterNoCase

  lookupRange k1 k2 (DmPT pt)
    = toIntermediates . SM.toList $ SM2.lookupRange k1 k2 pt

  unionWith op (DmPT pt1) (DmPT pt2)
    = mkDmPT $ SM.unionWith op pt1 pt2

{-
  unionWithConv to f (DmPT i1) (DmPT i2)
    = liftM mkDmPT $ unionWithConv to f i1 i2
-}

  map f (DmPT pt)
    = mkDmPT $ SM.map f pt

  mapMaybe f (DmPT pt)
    = mkDmPT $ SM.mapMaybe f pt

  keys (DmPT pt)
    = SM.keys pt

-- ------------------------------------------------------------
-- Inverted index using position proxy for geo coordinates
-- ------------------------------------------------------------

-- | Newtype to allow position normalization 'Bijection' instance.
newtype UnPos = UnPos { unPos :: Text }
  deriving (Show, Eq, NFData)

instance Bijection UnPos Text where
  to   = Pos.denormalize . unPos
  from = UnPos . Pos.normalize

instance Bijection Text UnPos where
  to   = UnPos
  from = unPos

-- ------------------------------------------------------------

-- ------------------------------------------------------------

-- | Geo-position index using a 'StringMap'-implementation.
--
newtype PrefixTreeIndexPosition
  = InvPosIx { invPosIx :: KeyProxyIndex Text (KeyProxyIndex UnPos (KeyProxyIndex Text (DmPrefixTree DocIdSet))) }
  deriving (Eq, Show, NFData, Typeable)

mkInvPosIx :: KeyProxyIndex Text (KeyProxyIndex UnPos (KeyProxyIndex Text (DmPrefixTree DocIdSet))) -> PrefixTreeIndexPosition
mkInvPosIx x = InvPosIx $! x

-- ------------------------------------------------------------

instance  Binary PrefixTreeIndexPosition where
  put = put . invPosIx
  get = get >>= return . mkInvPosIx

-- ------------------------------------------------------------

instance Index PrefixTreeIndexPosition where
  type IKey PrefixTreeIndexPosition = Word
  type IVal PrefixTreeIndexPosition = DocIdSet

  insertList wos (InvPosIx i)
    = mkInvPosIx $ insertList wos i

  deleteDocs docIds (InvPosIx i)
    = mkInvPosIx $ deleteDocs docIds i

  empty
    = mkInvPosIx $ empty

  fromList l
    = mkInvPosIx $ Ix.fromList l

  toList (InvPosIx i)
    = Ix.toList i

  search t k (InvPosIx i)
    = search t k i

  lookupRange k1 k2 (InvPosIx i)
    = lookupRange k1 k2 i

  unionWith op (InvPosIx i1) (InvPosIx i2)
    = mkInvPosIx $ unionWith op i1 i2

--  unionWithConv to' f (InvPosIx i1) (InvPosIx i2)
--    = mkInvPosIx $ unionWithConv to' f i1 i2

  map f (InvPosIx i)
    = mkInvPosIx $ Ix.map f i

  mapMaybe f (InvPosIx i)
    = mkInvPosIx $ Ix.mapMaybe f i

  keys (InvPosIx i)
    = keys i

