{-# OPTIONS -fno-warn-orphans     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}

{- |
  Default 'Bijection' instances.
-}

module Data.Bijection.Instances where

import           Data.Text

import           Data.Bijection

-- ----------------------------------------------------------------------------

-- | 'Text' to 'String'.
instance Bijection Text String where
  to   = unpack
  from = pack

-- | 'String' to 'Text'.
instance Bijection String Text where
  to   = pack
  from = unpack
