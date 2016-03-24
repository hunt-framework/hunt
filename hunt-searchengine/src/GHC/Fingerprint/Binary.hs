{-# OPTIONS -fno-warn-orphans #-}

-- ----------------------------------------------------------------------------
{- |
'Binary' instance for 'Fingerprint'.
-}
-- ----------------------------------------------------------------------------

module GHC.Fingerprint.Binary where

import           Data.Binary
import           GHC.Fingerprint.Type

-- ------------------------------------------------------------

instance Binary Fingerprint where
  put (Fingerprint hi lo) = put hi >> put lo
  get = Fingerprint <$> get <*> get

-- ------------------------------------------------------------
