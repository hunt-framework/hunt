{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances        #-}

-- ----------------------------------------------------------------------------
{- |
  Top-level index implementations with 'Text' keys for

    * text

    * integers

    * dates and

    * geographic positions.
-}
-- ----------------------------------------------------------------------------

module Hunt.Index.InvertedIndex
{-
  ( InvertedIndex (..)
  , InvertedIndexDate
  , InvertedIndexInt
  , InvertedIndexPosition
  , InvertedIndexRTree
  )
-- -}
where

import           Prelude                              as P

import           Control.DeepSeq

import           Data.Bijection
import           Data.Bijection.Instances             ()
import           Data.Binary                          (Binary (..))
import qualified Data.List                            as L
import           Data.Maybe                           (fromMaybe)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Typeable

import           Text.Read                            (readMaybe)

import           Hunt.Common.BasicTypes
import           Hunt.Common.Occurrences              (Occurrences)
import           Hunt.Index                           as Ix
import           Hunt.Index.PrefixTreeIndex
import qualified Hunt.Index.PrefixTreeIndex2Dim       as PT2D

import           Hunt.Index.Proxy.KeyIndex

import qualified Hunt.Index.Schema.Normalize.Date     as Date
import qualified Hunt.Index.Schema.Normalize.Int      as Int
import qualified Hunt.Index.Schema.Normalize.Position as Pos


-- ------------------------------------------------------------
-- Inverted index using text key
-- ------------------------------------------------------------

-- | Text index using a 'StringMap'-implementation.
newtype InvertedIndex
  = InvIx { invIx :: KeyProxyIndex Text (DmPrefixTree Occurrences) }
  deriving (Eq, Show, NFData, Typeable)

mkInvIx :: KeyProxyIndex Text (DmPrefixTree Occurrences)
        -> InvertedIndex
mkInvIx x = InvIx $! x

-- ------------------------------------------------------------

instance Binary InvertedIndex where
  put = put . invIx
  get = get >>= return . mkInvIx

-- ------------------------------------------------------------

instance Index InvertedIndex where
  type IKey InvertedIndex = Word
  type IVal InvertedIndex = Occurrences

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

  -- here the scoring of found word rel. to searched word is added
  searchSc t k m
      = L.map scoreWord $ search t k m
        where
          dist
              = similar k
          scoreWord (w, r)
              = (w, (dist w, r))


  lookupRange k1 k2 (InvIx i)
    = lookupRange k1 k2 i

  -- for lookupRangeSc the default scoring (all 1.0) is done
  --
  -- no better scoring known

  unionWith op (InvIx i1) (InvIx i2)
    = mkInvIx $ unionWith op i1 i2

{-
  unionWithConv to f (InvIx i1) (InvIx i2)
    = mkInvIx $ unionWithConv to f i1 i2
-}

  map f (InvIx i)
    = mkInvIx $ Ix.map f i

  mapMaybe f (InvIx i)
    = mkInvIx $ Ix.mapMaybe f i

  keys (InvIx i)
    = keys i


-- ------------------------------------------------------------

-- | a simple similarity heuristic for scoring words found
-- when doing a fuzzy or prefix search

similar :: Text -> Text -> Score
similar s f
    = -- traceShow ("similar"::Text, s, f, r) $
      r
    where
      r = similar' s f

similar' :: Text -> Text -> Score
similar' searched found
    | searched == found
        = 1.0
    | ls == lf
        = 0.75
    | ls < lf                     -- reduce score by length of found word
        = 0.5 * (fromIntegral ls / fromIntegral lf)
    | otherwise                   -- make similar total
        = noScore
    where
      ls = T.length searched
      lf = T.length found

-- ------------------------------------------------------------
-- Inverted index using 2-dimensional lookup
-- ------------------------------------------------------------

-- | Text index with 2-dimensional lookup using a 'StringMap'-implementation.
newtype InvertedIndex2Dim
  = InvIx2D { invIx2D :: KeyProxyIndex Text (PT2D.DmPrefixTree Occurrences) }
  deriving (Eq, Show, NFData, Typeable)

mkInvIx2D :: KeyProxyIndex Text (PT2D.DmPrefixTree Occurrences)
        -> InvertedIndex2Dim
mkInvIx2D x = InvIx2D $! x

-- ------------------------------------------------------------

instance Binary InvertedIndex2Dim where
  put = put . invIx2D
  get = get >>= return . mkInvIx2D

-- ------------------------------------------------------------

instance Index InvertedIndex2Dim where
  type IKey InvertedIndex2Dim = Word
  type IVal InvertedIndex2Dim = Occurrences

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

  -- TODO: searchSc and lookupRangeSc implementation similar to InvertedIndexInt and InvertedIndex

  lookupRange k1 k2 (InvIx2D i)
    = lookupRange k1 k2 i

  unionWith op (InvIx2D i1) (InvIx2D i2)
    = mkInvIx2D $ unionWith op i1 i2

{-
  unionWithConv to f (InvIx2D i1) (InvIx2D i2)
    = mkInvIx2D $ unionWithConv to f i1 i2
-}

  map f (InvIx2D i)
    = mkInvIx2D $ Ix.map f i

  mapMaybe f (InvIx2D i)
    = mkInvIx2D $ Ix.mapMaybe f i

  keys (InvIx2D i)
    = keys i


-- ------------------------------------------------------------
-- Inverted index using int proxy for numeric data
-- ------------------------------------------------------------

-- | Newtype to allow integer normalization 'Bijection' instance.
newtype UnInt = UnInt { unInt :: Text }
  deriving (Show, Eq, NFData)

instance Bijection UnInt Text where
  to   = Int.denormalizeFromText . unInt
  from = UnInt . Int.normalizeToText

instance Bijection Text UnInt where
  to   = UnInt
  from = unInt

-- ------------------------------------------------------------

-- | Integer index using a 'StringMap'-implementation.
newtype InvertedIndexInt
  = InvIntIx { invIntIx :: KeyProxyIndex Text (KeyProxyIndex UnInt InvertedIndex) }
  deriving (Eq, Show, NFData, Typeable)

mkInvIntIx :: KeyProxyIndex Text (KeyProxyIndex UnInt InvertedIndex) -> InvertedIndexInt
mkInvIntIx x = InvIntIx $! x

-- ------------------------------------------------------------

instance Binary InvertedIndexInt where
  put = put . invIntIx
  get = get >>= return . InvIntIx

-- ------------------------------------------------------------

instance Index InvertedIndexInt where
  type IKey InvertedIndexInt = Text
  type IVal InvertedIndexInt = IVal InvertedIndex

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

  searchSc t k m
      = L.map scoreWord $ search t k m
        where
          dist
              = similarInt k
          scoreWord (w, r)
              = (w, (dist w, r))

  lookupRange k1 k2 (InvIntIx i)
    = lookupRange k1 k2 i

  lookupRangeSc k1 k2 m
    = L.map scoreWord $ lookupRange k1 k2 m
      where
        dist
            = similarRangeInt k1 k2
        scoreWord (w, r)
            = (w, (dist w, r))

  unionWith op (InvIntIx i1) (InvIntIx i2)
    = mkInvIntIx $ unionWith op i1 i2

--  unionWithConv to' f (InvIntIx i1) (InvIntIx i2)
--    = mkInvIntIx $ unionWithConv to' f i1 i2

  map f (InvIntIx i)
    = mkInvIntIx $ Ix.map f i

  mapMaybe f (InvIntIx i)
    = mkInvIntIx $ Ix.mapMaybe f i

  keys (InvIntIx i)
    = keys i


similarInt :: Text -> Text -> Score
similarInt searched found
    = fromMaybe noScore $
      do s <- readMaybe $ T.unpack searched
         f <- readMaybe $ T.unpack found
         return $ similarFloat (fromIntegral (s::Int)) (fromIntegral (f::Int))

similarRangeInt :: Text -> Text -> Text -> Score
similarRangeInt lbt ubt found
    = fromMaybe noScore $
      do lb <- readMaybe $ T.unpack lbt
         ub <- readMaybe $ T.unpack ubt
         f  <- readMaybe $ T.unpack found
         return $ similarFloat
                    (fromIntegral ((lb::Int) + (ub::Int)) / 2.0)
                    (fromIntegral (f::Int))

similarFloat :: Float -> Float -> Score
similarFloat mu
    = mkScore . bellCurve (sigma mu) mu

sigma :: Float -> Float
sigma x
    = abs x `max` 10.0 / 10.0

-- | Gaussian bell curve for scoring
bellCurve :: Float -> (Float -> Float -> Float)
bellCurve sigma'
    = \ mu x -> exp (- (x - mu) ^ _2 / sigma2'2)
    where
      _2 :: Int
      _2 = 2
      sigma2'2 = 2.0 * sigma' ^ _2

-- ------------------------------------------------------------
-- inverted index using date proxy for dates
-- ------------------------------------------------------------

-- | Newtype to allow date normalization 'Bijection' instance.
newtype UnDate = UnDate { unDate :: Text }
  deriving (Show, Eq, NFData)

instance Bijection UnDate Text where
  to   = Date.denormalize . unDate
  from = UnDate . Date.normalize

instance Bijection Text UnDate where
  to   = UnDate
  from = unDate

-- ------------------------------------------------------------

-- | Date index using a 'StringMap'-implementation.
newtype InvertedIndexDate
  = InvDateIx { invDateIx :: KeyProxyIndex Text (KeyProxyIndex UnDate InvertedIndex) }
  deriving (Eq, Show, NFData, Typeable)

mkInvDateIx :: KeyProxyIndex Text (KeyProxyIndex UnDate InvertedIndex) -> InvertedIndexDate
mkInvDateIx x = InvDateIx $! x

-- ------------------------------------------------------------

instance Binary InvertedIndexDate where
  put = put . invDateIx
  get = get >>= return . mkInvDateIx

-- ------------------------------------------------------------

instance Index InvertedIndexDate where
  type IKey InvertedIndexDate = Word
  type IVal InvertedIndexDate = IVal InvertedIndex

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

  -- TODO: searchSc and lookupRangeSc implementation similar to InvertedIndexInt and InvertedIndex

  lookupRange k1 k2 (InvDateIx i)
    = lookupRange k1 k2 i

  unionWith op (InvDateIx i1) (InvDateIx i2)
    = mkInvDateIx $ unionWith op i1 i2

--  unionWithConv to' f (InvDateIx i1) (InvDateIx i2)
--    = mkInvDateIx $ unionWithConv to' f i1 i2

  map f (InvDateIx i)
    = mkInvDateIx $ Ix.map f i

  mapMaybe f (InvDateIx i)
    = mkInvDateIx $ Ix.mapMaybe f i

  keys (InvDateIx i)
    = keys i


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

-- | Geo-position index using a 'StringMap'-implementation.
newtype InvertedIndexPosition
  = InvPosIx { invPosIx :: KeyProxyIndex Text (KeyProxyIndex UnPos InvertedIndex2Dim) }
  deriving (Eq, Show, NFData, Typeable)

mkInvPosIx :: KeyProxyIndex Text (KeyProxyIndex UnPos InvertedIndex2Dim) -> InvertedIndexPosition
mkInvPosIx x = InvPosIx $! x

-- ------------------------------------------------------------

instance  Binary InvertedIndexPosition where
  put = put . invPosIx
  get = get >>= return . mkInvPosIx

-- ------------------------------------------------------------

instance Index InvertedIndexPosition where
  type IKey InvertedIndexPosition = Word
  type IVal InvertedIndexPosition = IVal InvertedIndex2Dim

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

--  unionWithConv to' f (InvPosIx i1) (InvPosIx i2)
--    = mkInvPosIx $ unionWithConv to' f i1 i2

  map f (InvPosIx i)
    = mkInvPosIx $ Ix.map f i

  mapMaybe f (InvPosIx i)
    = mkInvPosIx $ Ix.mapMaybe f i

  keys (InvPosIx i)
    = keys i

