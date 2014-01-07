{-# OPTIONS -fno-warn-orphans     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}

module Data.Bijection.Instances where

import           Data.Text

import           Data.Bijection

-- ----------------------------------------------------------------------------

-- | 'Text' to 'String'.
instance Bijection Text String where
  to   = unpack
  from = pack

-- | 'String' to 'Text'.
--   See comments in 'Data.Bijection' for why both instances need to be defined.
instance Bijection String Text where
  to   = pack
  from = unpack
