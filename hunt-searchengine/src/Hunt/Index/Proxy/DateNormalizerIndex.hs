{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE TypeFamilies               #-}

module Hunt.Index.Proxy.DateNormalizerIndex
(
  -- normalized date in textual representation to internal format
  DateNormalizerIndex(..)
)
where

import           Prelude                                    as P

import           Control.Applicative                        ((<$>))
import           Control.Arrow                              (first)
import           Control.DeepSeq
import           Control.Monad

import           Data.Binary                                (Binary (..))
import           Data.Text                                  (Text)

import           Hunt.Index.Index
import qualified Hunt.Index.Index                       as Ix
import qualified Hunt.Index.Schema.Normalize.Date       as Date

-- ----------------------------------------------------------------------------
-- Int values represented as Int (Text to Int conversion)
-- ----------------------------------------------------------------------------

newtype DateNormalizerIndex impl cv
    = DateNIx { dateNIx :: impl cv}
    deriving (Eq, Show, NFData)

mkDateNIx :: impl cv -> DateNormalizerIndex impl cv
mkDateNIx v = DateNIx $! v

-- ----------------------------------------------------------------------------

instance Binary (impl v) => Binary (DateNormalizerIndex impl v) where
    put = put . dateNIx
    get = get >>= return . mkDateNIx

-- ----------------------------------------------------------------------------

-- | NOTE: Validation need to be performed before this proxy is applied
instance Index (DateNormalizerIndex impl) where
    type IKey      (DateNormalizerIndex impl) v = Text
    type IVal      (DateNormalizerIndex impl) v = IVal impl v
    type ICon      (DateNormalizerIndex impl) v =
        ( Index impl
        , ICon impl v
        , IKey impl v ~ Text
        )

    batchInsert kvs (DateNIx i)
        = liftM mkDateNIx $ batchInsert (P.map (first Date.normalize) kvs) i

    batchDelete ks (DateNIx i)
        = liftM mkDateNIx $ batchDelete ks i

    empty
        = mkDateNIx $ empty

    fromList l
        = liftM mkDateNIx . fromList $ P.map (first Date.normalize) l

    toList (DateNIx i)
        = liftM (first Date.denormalize <$>) $ toList i

    search t k (DateNIx i)
        = liftM (first Date.denormalize <$>) $ search t (Date.normalize k) i

    lookupRange k1 k2 (DateNIx i)
        = liftM (first Date.denormalize <$>) $ lookupRange (Date.normalize k1) (Date.normalize k2) i

    unionWith op (DateNIx i1) (DateNIx i2)
        = liftM mkDateNIx $ unionWith op i1 i2

    unionWithConv to f (DateNIx i1) (DateNIx i2)
        = liftM mkDateNIx $ unionWithConv to f i1 i2

    map f (DateNIx i)
        = liftM mkDateNIx $ Ix.map f i

    keys (DateNIx i)
        = liftM (P.map Date.denormalize) $ keys i

-- ----------------------------------------------------------------------------
