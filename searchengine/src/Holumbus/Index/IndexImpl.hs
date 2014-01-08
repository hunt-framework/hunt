{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}

module Holumbus.Index.IndexImpl where

import           Data.Binary
import           Data.Text                      (Text)

import           Holumbus.Common
import           Holumbus.Index.Index

-- ------------------------------------------------------------

type IndexImplCon i v
  = ( Index i
    , IKey i v ~ Text
    , IVal i v ~ v
    , ICon i v
    , Show (i v)
    , Binary (i v)
    )

-- ------------------------------------------------------------

data IndexImpl v
  = forall i. IndexImplCon i v => IndexImpl { ixImpl :: i v }

data ContextMeta = CxMeta
  { cmType   :: CType
  , cmIxImpl :: IndexImpl Occurrences
  }

-- ------------------------------------------------------------

deriving instance Show (IndexImpl v)

-- | FIXME: actually implement instance
instance Binary (IndexImpl v) where
  put (IndexImpl i) = put i
  get = undefined

-- ------------------------------------------------------------

-- | IndexImpl is the Wrapper for external access
--   we set the key to Text here, but allow internal
--   Key in all haskell types. For conversion we have
--   could imagine a normalization proxy implemented
--   with the KeyIndex Proxy
mkIndex :: IndexImplCon i v  => i v -> IndexImpl v
mkIndex i = IndexImpl $! i
