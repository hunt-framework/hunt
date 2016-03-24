{-# OPTIONS -fno-warn-orphans #-}

-- ----------------------------------------------------------------------------
{- |
'Binary' instance for 'TypeRep'.
-}
-- ----------------------------------------------------------------------------

module Data.Typeable.Binary where

import           Data.Binary
import           Data.Typeable.Internal
import           GHC.Fingerprint.Binary ()

-- ------------------------------------------------------------

instance Binary TypeRep where
  put (TypeRep fp tyCon ks ts) = put fp >> put tyCon >> put ks >> put ts
  get = TypeRep <$> get <*> get <*> get <*> get

instance Binary TyCon where
  put (TyCon hash package modul name) = put hash >> put package >> put modul >> put name
  get = TyCon <$> get <*> get <*> get <*> get

-- ------------------------------------------------------------
