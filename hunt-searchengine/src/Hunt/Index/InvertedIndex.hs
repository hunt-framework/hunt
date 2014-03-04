{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Hunt.Index.InvertedIndex
( InvertedIndex(..)
, InvertedIndexDate
, InvertedIndexInt
, InvertedIndexPosition
)
where

import           Prelude                                    as P

import           Control.DeepSeq

import           Data.Bijection.Instances                   ()
import           Data.Binary                                (Binary (..))
import           Data.Text                                  (Text)
import           Data.Typeable

import           Hunt.Common.BasicTypes
import           Hunt.Common.Occurrences                    (Occurrences)
import           Hunt.Common.Occurrences.Compression.BZip
import           Hunt.Common.Occurrences.Compression
import           Hunt.Index.ComprPrefixTreeIndex
import qualified Hunt.Index.ComprPrefixTreeIndex2Dim        as PT2D
import           Hunt.Index.Index                           as Ix

import           Hunt.Index.Proxy.KeyIndex

import qualified Hunt.Index.Schema.Normalize.Int            as Int
import qualified Hunt.Index.Schema.Normalize.Date           as Date
import qualified Hunt.Index.Schema.Normalize.Position       as Pos

import           Data.Bijection

-- ----------------------------------------------------------------------------
-- inverted index using int proxy for numeric data
-- ----------------------------------------------------------------------------

-- newtype required to enable different Text->Text Bijection instances
newtype UnInt = UnInt { unInt :: Text }
  deriving (Show, Eq, NFData)

instance Bijection UnInt Text where
  to   = Int.denormalizeFromText . unInt
  from = UnInt . Int.normalizeToText

instance Bijection Text UnInt where
  to   = UnInt
  from = unInt

newtype InvertedIndexInt v
  = InvIntIx { invIntIx :: KeyProxyIndex Text (KeyProxyIndex UnInt InvertedIndex) v }
  deriving (Eq, Show, NFData, Typeable)

mkInvIntIx :: KeyProxyIndex Text (KeyProxyIndex UnInt InvertedIndex) v -> InvertedIndexInt v
mkInvIntIx x = InvIntIx $! x

-- ----------------------------------------------------------------------------

instance Binary (InvertedIndexInt v) where
  put = put . invIntIx
  get = get >>= return . InvIntIx

-- ----------------------------------------------------------------------------

instance Index InvertedIndexInt where
  type IKey InvertedIndexInt v = Text
  type IVal InvertedIndexInt v = Occurrences

  insertList wos (InvIntIx i)
    = mkInvIntIx $ insertList wos i

  deleteDocs docIds (InvIntIx i)
    = mkInvIntIx $ deleteDocs docIds i

  empty
    = mkInvIntIx $ empty

  fromList l
    = mkInvIntIx $ fromList l

  toList (InvIntIx i)
    = toList i

  search t k (InvIntIx i)
    = search t k i

  lookupRange k1 k2 (InvIntIx i)
    = lookupRange k1 k2 i

  unionWith op (InvIntIx i1) (InvIntIx i2)
    = mkInvIntIx $ unionWith op i1 i2

  unionWithConv to' f (InvIntIx i1) (InvIntIx i2)
    = mkInvIntIx $ unionWithConv to' f i1 i2

  map f (InvIntIx i)
    = mkInvIntIx $ Ix.map f i

  keys (InvIntIx i)
    = keys i


-- ----------------------------------------------------------------------------
-- inverted index using date proxy for date information
-- ----------------------------------------------------------------------------
newtype UnDate = UnDate { unDate :: Text }
  deriving (Show, Eq, NFData)

instance Bijection UnDate Text where
  to   = Date.denormalize . unDate
  from = UnDate . Date.normalize

instance Bijection Text UnDate where
  to   = UnDate
  from = unDate

newtype InvertedIndexDate v
  = InvDateIx { invDateIx :: KeyProxyIndex Text (KeyProxyIndex UnDate InvertedIndex) v }
  deriving (Eq, Show, NFData, Typeable)

mkInvDateIx :: KeyProxyIndex Text (KeyProxyIndex UnDate InvertedIndex) v -> InvertedIndexDate v
mkInvDateIx x = InvDateIx $! x

-- ----------------------------------------------------------------------------

instance Binary (InvertedIndexDate v) where
  put = put . invDateIx
  get = get >>= return . mkInvDateIx

-- ----------------------------------------------------------------------------

instance Index InvertedIndexDate where
  type IKey InvertedIndexDate v = Word
  type IVal InvertedIndexDate v = Occurrences

  insertList wos (InvDateIx i)
    = mkInvDateIx $ insertList wos i

  deleteDocs docIds (InvDateIx i)
    = mkInvDateIx $ deleteDocs docIds i

  empty
    = mkInvDateIx $ empty

  fromList l
    = mkInvDateIx $ fromList l

  toList (InvDateIx i)
    = toList i

  search t k (InvDateIx i)
    = search t k i

  lookupRange k1 k2 (InvDateIx i)
    = lookupRange k1 k2 i

  unionWith op (InvDateIx i1) (InvDateIx i2)
    = mkInvDateIx $ unionWith op i1 i2

  unionWithConv to' f (InvDateIx i1) (InvDateIx i2)
    = mkInvDateIx $ unionWithConv to' f i1 i2

  map f (InvDateIx i)
    = mkInvDateIx $ Ix.map f i

  keys (InvDateIx i)
    = keys i

-- ----------------------------------------------------------------------------
-- inverted index using position proxy for geo coordinates
-- ----------------------------------------------------------------------------
newtype UnPos = UnPos { unPos :: Text }
  deriving (Show, Eq, NFData)

instance Bijection UnPos Text where
  to   = Pos.denormalize . unPos
  from = UnPos . Pos.normalize

instance Bijection Text UnPos where
  to   = UnPos
  from = unPos

newtype InvertedIndexPosition v
  = InvPosIx { invPosIx :: KeyProxyIndex Text (KeyProxyIndex UnPos InvertedIndex2Dim) v }
  deriving (Eq, Show, NFData, Typeable)

mkInvPosIx :: KeyProxyIndex Text (KeyProxyIndex UnPos InvertedIndex2Dim) v -> InvertedIndexPosition v
mkInvPosIx x = InvPosIx $! x

-- ----------------------------------------------------------------------------

instance Binary (InvertedIndexPosition v) where
  put = put . invPosIx
  get = get >>= return . mkInvPosIx

-- ----------------------------------------------------------------------------

instance Index InvertedIndexPosition where
  type IKey InvertedIndexPosition v = Word
  type IVal InvertedIndexPosition v = Occurrences

  insertList wos (InvPosIx i)
    = mkInvPosIx $ insertList wos i

  deleteDocs docIds (InvPosIx i)
    = mkInvPosIx $ deleteDocs docIds i

  empty
    = mkInvPosIx $ empty

  fromList l
    = mkInvPosIx $ fromList l

  toList (InvPosIx i)
    = toList i

  search t k (InvPosIx i)
    = search t k i

  lookupRange k1 k2 (InvPosIx i)
    = lookupRange k1 k2 i

  unionWith op (InvPosIx i1) (InvPosIx i2)
    = mkInvPosIx $ unionWith op i1 i2

  unionWithConv to' f (InvPosIx i1) (InvPosIx i2)
    = mkInvPosIx $ unionWithConv to' f i1 i2

  map f (InvPosIx i)
    = mkInvPosIx $ Ix.map f i

  keys (InvPosIx i)
    = keys i


-- ----------------------------------------------------------------------------
-- default inverted index using text key
-- ----------------------------------------------------------------------------

newtype InvertedIndex _v
  = InvIx { invIx :: KeyProxyIndex Text ComprOccPrefixTree CompressedOccurrences }
  deriving (Eq, Show, NFData, Typeable)

mkInvIx :: KeyProxyIndex Text ComprOccPrefixTree CompressedOccurrences
        -> InvertedIndex _v
mkInvIx x = InvIx $! x

-- ----------------------------------------------------------------------------

instance Binary (InvertedIndex v) where
  put = put . invIx
  get = get >>= return . mkInvIx

-- ----------------------------------------------------------------------------

instance Index InvertedIndex where
  type IKey InvertedIndex v = Word
  type IVal InvertedIndex v = Occurrences

  insertList wos (InvIx i)
    = mkInvIx $ insertList wos i

  deleteDocs docIds (InvIx i)
    = mkInvIx $ deleteDocs docIds i

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

  unionWithConv
    = error "InvertedIndex unionWithConv: cannot be used there because type variable v is fixed"
{-
  unionWithConv to f (InvIx i1) (InvIx i2)
    = mkInvIx $ unionWithConv to f i1 i2
-}

  map f (InvIx i)
    = mkInvIx $ Ix.map f i

  keys (InvIx i)
    = keys i

-- ----------------------------------------------------------------------------
-- 2dim inverted index using text key
-- ----------------------------------------------------------------------------

newtype InvertedIndex2Dim _v
  = InvIx2D { invIx2D :: KeyProxyIndex Text PT2D.ComprOccPrefixTree CompressedOccurrences }
  deriving (Eq, Show, NFData, Typeable)

mkInvIx2D :: KeyProxyIndex Text PT2D.ComprOccPrefixTree CompressedOccurrences
        -> InvertedIndex2Dim _v
mkInvIx2D x = InvIx2D $! x

-- ----------------------------------------------------------------------------

instance Binary (InvertedIndex2Dim v) where
  put = put . invIx2D
  get = get >>= return . mkInvIx2D

-- ----------------------------------------------------------------------------

instance Index InvertedIndex2Dim where
  type IKey InvertedIndex2Dim v = Word
  type IVal InvertedIndex2Dim v = Occurrences

  insertList wos (InvIx2D i)
    = mkInvIx2D $ insertList wos i

  deleteDocs docIds (InvIx2D i)
    = mkInvIx2D $ deleteDocs docIds i

  empty
    = mkInvIx2D $ empty

  fromList l
    = mkInvIx2D $ fromList l

  toList (InvIx2D i)
    = toList i

  search t k (InvIx2D i)
    = search t k i

  lookupRange k1 k2 (InvIx2D i)
    = lookupRange k1 k2 i

  unionWith op (InvIx2D i1) (InvIx2D i2)
    = mkInvIx2D $ unionWith op i1 i2

  unionWithConv
    = error "InvertedIndex2Dim unionWithConv: cannot be used there because type variable v is fixed"
{-
  unionWithConv to f (InvIx2D i1) (InvIx2D i2)
    = mkInvIx2D $ unionWithConv to f i1 i2
-}

  map f (InvIx2D i)
    = mkInvIx2D $ Ix.map f i

  keys (InvIx2D i)
    = keys i
