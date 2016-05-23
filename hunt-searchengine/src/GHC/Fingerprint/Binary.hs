{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

-- ----------------------------------------------------------------------------
{- |
'Binary' instance for 'Fingerprint'.
-}
-- ----------------------------------------------------------------------------

module GHC.Fingerprint.Binary where

#if !(MIN_VERSION_binary(0,7,6))

import           Data.Binary
import           GHC.Fingerprint.Type

-- ------------------------------------------------------------

instance Binary Fingerprint where
  put (Fingerprint hi lo) = put hi >> put lo
  get = Fingerprint <$> get <*> get

-- ------------------------------------------------------------

#endif
