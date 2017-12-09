{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-orphans #-}

-- ----------------------------------------------------------------------------
{- |
'Binary' instance for 'TypeRep'.
-}
-- ----------------------------------------------------------------------------

module Data.Typeable.Binary where

import           Data.Binary
import           GHC.Fingerprint.Binary ()

-- ------------------------------------------------------------

#if MIN_VERSION_binary(0,8,5)

-- Recent binary versions provide a Binary instance of TypeRep!

#elif __GLASGOW_HASKELL__ < 800

import           Data.Typeable.Internal

instance Binary TypeRep where
  put (TypeRep fp tyCon kindRep tr) = put fp >> put tyCon >> put kindRep >> put tr
  get = TypeRep <$> get <*> get <*> get <*> get

instance Binary TyCon where
  put (TyCon hash package modul name) = put hash >> put package >> put modul >> put name
  get = TyCon <$> get <*> get <*> get <*> get

#else

import           Data.Typeable.Internal

instance Binary TypeRep where
  put (TypeRep fp tyCon kindRep tr) = put fp >> put tyCon >> put kindRep >> put tr
  get = TypeRep <$> get <*> get <*> get <*> get

instance Binary TyCon where
  put tc = put (tyConPackage tc) >> put (tyConModule tc) >> put (tyConName tc)
  get = mkTyCon3 <$> get <*> get <*> get

#endif

-- ------------------------------------------------------------
