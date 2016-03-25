{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- ----------------------------------------------------------------------------

{- |
  Key conversion proxy.
  Wraps an index to expose the desired key type.
  The conversion is defined by the 'Bijection' implementation.

  This can be used for simple conversions like @Text@ to @String@ or normalization and compression.
-}

-- ----------------------------------------------------------------------------

module Hunt.Index.Proxy.KeyIndex
       ( KeyProxyIndex (..)
       )
where

import           Control.Arrow (first)
import           Control.DeepSeq
import           Data.Bijection
import           Data.Binary (Binary (..))
import           Hunt.Index
import qualified Hunt.Index as Ix
import           Prelude as P

-- ------------------------------------------------------------

-- | Key conversion proxy.
--   @toType@ is the desired/exposed key type, followed by the wrapped index type.
--   There has to be a corresponding 'Bijection' instance:
--
--   >  instance Bijection (IKey impl v) toType where ...
newtype KeyProxyIndex toType impl
  = KeyProxyIx { keyProxyIx :: impl }
  deriving (Eq, Show, NFData)

-- | Wrap an index in a key conversion proxy.
mkKeyProxyIx :: impl -> KeyProxyIndex toType impl
mkKeyProxyIx v = KeyProxyIx $! v

-- ------------------------------------------------------------

instance Binary (impl) => Binary (KeyProxyIndex toType impl) where
  put = put . keyProxyIx
  get = get >>= return . mkKeyProxyIx

-- ------------------------------------------------------------

instance (IndexValue (IVal impl)) => Index (KeyProxyIndex toType impl) where
  type IKey      (KeyProxyIndex toType impl) = toType
  type IVal      (KeyProxyIndex toType impl) = IVal impl
  type ICon      (KeyProxyIndex toType impl) = ( Index impl
                                               , ICon impl
                                               , Bijection (IKey impl) toType
                                               )

  insertList kvs (KeyProxyIx i)
    = mkKeyProxyIx $ insertList (P.map (first from) kvs) i

  deleteDocs ks (KeyProxyIx i)
    = mkKeyProxyIx $ deleteDocs ks i

  empty
    = mkKeyProxyIx $ empty

  fromList l
    = mkKeyProxyIx . fromList $ P.map (first from) l

  toList (KeyProxyIx i)
    = first to <$> toList i

  search t k (KeyProxyIx i)
    = first to <$> search t (from k) i

  lookupRange k1 k2 (KeyProxyIx i)
    = first to <$> lookupRange (from k1) (from k2) i

  unionWith op (KeyProxyIx i1) (KeyProxyIx i2)
    = mkKeyProxyIx $ unionWith op i1 i2

--  unionWithConv t f (KeyProxyIx i1) (KeyProxyIx i2)
--    = mkKeyProxyIx $ unionWithConv t f i1 i2

  map f (KeyProxyIx i)
    = mkKeyProxyIx $ Ix.map f i

  mapMaybe f (KeyProxyIx i)
    = mkKeyProxyIx $ Ix.mapMaybe f i

  keys (KeyProxyIx i)
    = P.map to $ keys i

-- ------------------------------------------------------------
