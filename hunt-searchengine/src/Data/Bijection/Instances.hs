{-# OPTIONS -fno-warn-orphans     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- ----------------------------------------------------------------------------
{- |
  Default 'Bijection' instances.
-}
-- ----------------------------------------------------------------------------

module Data.Bijection.Instances where

import Data.Bijection
import Data.Text

-- ------------------------------------------------------------

-- | 'Text' to 'String'.
instance Bijection Text String where
  to   = unpack
  from = pack

-- | 'String' to 'Text'.
instance Bijection String Text where
  to   = pack
  from = unpack

-- ------------------------------------------------------------
