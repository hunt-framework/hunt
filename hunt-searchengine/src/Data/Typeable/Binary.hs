{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-orphans #-}

-- ----------------------------------------------------------------------------
{- |
'Binary' instance for 'TypeRep'.
-}
-- ----------------------------------------------------------------------------

module Data.Typeable.Binary where

import           Control.Applicative

import           Data.Binary
import           Data.Typeable.Internal

import           GHC.Fingerprint.Binary ()

-- ------------------------------------------------------------

#if __GLASGOW_HASKELL__ < 710

instance Binary TypeRep where
  put (TypeRep fp tyCon ts) = put fp >> put tyCon >> put ts
  get = TypeRep <$> get <*> get <*> get

instance Binary TyCon where
  put (TyCon hash package modul name) = put hash >> put package >> put modul >> put name
  get = TyCon <$> get <*> get <*> get <*> get

#else

instance Binary TypeRep where
  put (TypeRep fp tyCon kindRep typeRep) = put fp >> put tyCon >> put kindRep >> put typeRep
  get = TypeRep <$> get <*> get <*> get <*> get

instance Binary TyCon where
  put (TyCon hash package modul name) = put hash >> put package >> put modul >> put name
  get = TyCon <$> get <*> get <*> get <*> get

#endif

-- ------------------------------------------------------------
