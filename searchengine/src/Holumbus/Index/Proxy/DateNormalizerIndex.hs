{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}

module Holumbus.Index.Proxy.DateNormalizerIndex
(
  -- normalized date in textual representation to internal format
  DateNormalizerIndex(..)
)
where

import           Prelude                                    as P
import           Control.DeepSeq

import           Control.Applicative                        ((<$>))
import           Control.Arrow                              (first)

import           Data.Binary                                (Binary (..))
import           Data.Text                                  (Text)

import           Holumbus.Index.Index
import qualified Holumbus.Index.Index                       as Ix
import qualified Holumbus.Index.Schema.Normalize.Date       as Date

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
    type ISearchOp (DateNormalizerIndex impl) v = ISearchOp impl v
    type ICon      (DateNormalizerIndex impl) v =
        ( Index impl
        , ICon impl v
        , IKey impl v ~ Text
        )

    insert k v (DateNIx i)
        = mkDateNIx $ insert (Date.normalize k) v i

    batchDelete ks (DateNIx i)
        = mkDateNIx $ batchDelete ks i

    empty
        = mkDateNIx $ empty

    fromList l
        = mkDateNIx . fromList $ P.map (first Date.normalize) l

    toList (DateNIx i)
        = first Date.denormalize <$> toList i

    search t k (DateNIx i)
        = first Date.denormalize <$> search t (Date.normalize k) i

    lookupRange k1 k2 (DateNIx i)
        = first Date.denormalize <$> lookupRange (Date.normalize k1) (Date.normalize k2) i

    unionWith op (DateNIx i1) (DateNIx i2)
        = mkDateNIx $ unionWith op i1 i2

    map f (DateNIx i)
        = mkDateNIx $ Ix.map f i

    keys (DateNIx i)
        = P.map Date.denormalize $ keys i

-- ----------------------------------------------------------------------------
