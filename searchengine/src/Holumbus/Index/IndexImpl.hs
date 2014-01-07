{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Holumbus.Index.IndexImpl where

import           Holumbus.Index.Index
import           Holumbus.Common
import           Data.Text                      (Text)
import           Data.Binary



type IndexImplCon i v = ( Index i
                        , IKey i v ~ Text
                        , IVal i v ~ v
                        , ICon i v
                        , ISearchOp i v ~ TextSearchOp
                        , Binary (i v)
                        )

data IndexImpl v 
  = forall i. IndexImplCon i v => IndexImpl { ixImpl :: i v } 

-- | XXX actually implement instance
instance Show (IndexImpl i) where
  show _ = "x" 

-- | XXX actually implement instance
instance Binary (IndexImpl i) where
  put (IndexImpl i) = put i 
  get = undefined

-- | IndexImpl is the Wrapper for external access
--   we set the key to Text here, but allow internal
--   Key in all haskell types. For conversion we have
--   could imagine a normalization proxy implemented
--   with the KeyIndex Proxy
mkIndex      :: IndexImplCon i v  => i v -> IndexImpl v 
mkIndex i    = IndexImpl $! i


data ContextMeta = CxMeta 
   { cmType   :: CType
   , cmIxImpl :: IndexImpl Occurrences
   } 


