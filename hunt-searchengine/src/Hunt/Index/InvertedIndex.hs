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

import           Data.Bijection.Instances             ()
import           Data.Binary                          (Binary (..))
import qualified Data.List                            as L
import           Data.Text                            (Text)
import           Data.Typeable

import           Hunt.Common.BasicTypes
import           Hunt.Common.Occurrences              (Occurrences)
import           Hunt.Index                           as Ix
import           Hunt.Index.PrefixTreeIndex
import qualified Hunt.Index.PrefixTreeIndex2Dim       as PT2D
import           Hunt.Scoring.Keys                    (similar)

import           Hunt.Index.Proxy.KeyIndex


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
          scoreWord (w, r)
              = (w, (similar k w, r))


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


