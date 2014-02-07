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
import           Control.Monad

import           Data.Bijection.Instances                   ()
import           Data.Binary                                (Binary (..))
import           Data.Text                                  (Text)
import           Data.Typeable

import           Hunt.Common.BasicTypes
import           Hunt.Common.Occurrences                    (Occurrences)
import           Hunt.Common.Occurrences.Compression.Snappy

import           Hunt.Index.ComprPrefixTreeIndex
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
  to = Int.normalizeToText . unInt
  from = UnInt . Int.denormalizeFromText

instance Bijection Text UnInt where
  to = UnInt
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

    batchInsert wos (InvIntIx i)
        = liftM mkInvIntIx $ batchInsert wos i

    batchDelete docIds (InvIntIx i)
        = liftM mkInvIntIx $ batchDelete docIds i

    empty
        = mkInvIntIx $ empty

    fromList l
        = liftM mkInvIntIx $ fromList l

    toList (InvIntIx i)
        = toList i

    search t k (InvIntIx i)
        = search t k i

    lookupRange k1 k2 (InvIntIx i)
        = lookupRange k1 k2 i

    unionWith op (InvIntIx i1) (InvIntIx i2)
        = liftM mkInvIntIx $ unionWith op i1 i2

    unionWithConv to' f (InvIntIx i1) (InvIntIx i2)
        = liftM mkInvIntIx $ unionWithConv to' f i1 i2

    map f (InvIntIx i)
        = liftM mkInvIntIx $ Ix.map f i

    keys (InvIntIx i)
        = keys i


-- ----------------------------------------------------------------------------
-- inverted index using date proxy for date information
-- ----------------------------------------------------------------------------
newtype UnDate = UnDate { unDate :: Text }
  deriving (Show, Eq, NFData) 

instance Bijection UnDate Text where
  to = Date.normalize . unDate
  from = UnDate . Date.denormalize

instance Bijection Text UnDate where
  to = UnDate
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

    batchInsert wos (InvDateIx i)
        = liftM mkInvDateIx $ batchInsert wos i

    batchDelete docIds (InvDateIx i)
        = liftM mkInvDateIx $ batchDelete docIds i

    empty
        = mkInvDateIx $ empty

    fromList l
        = liftM mkInvDateIx $ fromList l

    toList (InvDateIx i)
        = toList i

    search t k (InvDateIx i)
        = search t k i

    lookupRange k1 k2 (InvDateIx i)
        = lookupRange k1 k2 i

    unionWith op (InvDateIx i1) (InvDateIx i2)
        = liftM mkInvDateIx $ unionWith op i1 i2

    unionWithConv to' f (InvDateIx i1) (InvDateIx i2)
        = liftM mkInvDateIx $ unionWithConv to' f i1 i2

    map f (InvDateIx i)
        = liftM mkInvDateIx $ Ix.map f i

    keys (InvDateIx i)
        = keys i

-- ----------------------------------------------------------------------------
-- inverted index using position proxy for geo coordinates
-- ----------------------------------------------------------------------------
newtype UnPos = UnPos { unPos :: Text }
  deriving (Show, Eq, NFData) 

instance Bijection UnPos Text where
  to = Pos.normalize . unPos
  from = UnPos . Pos.denormalize

instance Bijection Text UnPos where
  to = UnPos
  from = unPos

newtype InvertedIndexPosition v
    = InvPosIx { invPosIx :: KeyProxyIndex Text (KeyProxyIndex UnPos InvertedIndex) v }
    deriving (Eq, Show, NFData, Typeable)

mkInvPosIx :: KeyProxyIndex Text (KeyProxyIndex UnPos InvertedIndex) v  -> InvertedIndexPosition v
mkInvPosIx x = InvPosIx $! x

-- ----------------------------------------------------------------------------

instance Binary (InvertedIndexPosition v) where
    put = put . invPosIx
    get = get >>= return . mkInvPosIx

-- ----------------------------------------------------------------------------

instance Index InvertedIndexPosition where
    type IKey InvertedIndexPosition v = Word
    type IVal InvertedIndexPosition v = Occurrences

    batchInsert wos (InvPosIx i)
        = liftM mkInvPosIx $ batchInsert wos i

    batchDelete docIds (InvPosIx i)
        = liftM mkInvPosIx $ batchDelete docIds i

    empty
        = mkInvPosIx $ empty

    fromList l
        = liftM mkInvPosIx $ fromList l

    toList (InvPosIx i)
        = toList i

    search t k (InvPosIx i)
        = search t k i

    lookupRange k1 k2 (InvPosIx i)
        = lookupRange k1 k2 i

    unionWith op (InvPosIx i1) (InvPosIx i2)
        = liftM mkInvPosIx $ unionWith op i1 i2

    unionWithConv to' f (InvPosIx i1) (InvPosIx i2)
        = liftM mkInvPosIx $ unionWithConv to' f i1 i2

    map f (InvPosIx i)
        = liftM mkInvPosIx $ Ix.map f i

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

    batchInsert wos (InvIx i)
        = liftM mkInvIx $ batchInsert wos i

    batchDelete docIds (InvIx i)
        = liftM mkInvIx $ batchDelete docIds i

    empty
        = mkInvIx $ empty

    fromList l
        = liftM mkInvIx $ fromList l

    toList (InvIx i)
        = toList i

    search t k (InvIx i)
        = search t k i

    lookupRange k1 k2 (InvIx i)
        = lookupRange k1 k2 i

    unionWith op (InvIx i1) (InvIx i2)
        = liftM mkInvIx $ unionWith op i1 i2

    unionWithConv
        = error "InvertedIndex unionWithConv: cannot be used there because type variable v is fixed"
{-
    unionWithConv to f (InvIx i1) (InvIx i2)
        = liftM mkInvIx $ unionWithConv to f i1 i2
-}

    map f (InvIx i)
        = liftM mkInvIx $ Ix.map f i

    keys (InvIx i)
        = keys i
