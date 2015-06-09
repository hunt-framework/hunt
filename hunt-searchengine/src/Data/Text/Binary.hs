{-# OPTIONS -fno-warn-orphans #-}

-- ----------------------------------------------------------------------------
{- |
Default 'Binary' instance for 'Text' using UTF-8.
-}
-- ----------------------------------------------------------------------------

module Data.Text.Binary
where

import           Control.Monad      (liftM)
import           Data.Binary        (Binary (..))
import           Data.Text
import           Data.Text.Encoding

-- ------------------------------------------------------------

instance Binary Text where
  put = put . encodeUtf8
  get = liftM decodeUtf8 get

-- ------------------------------------------------------------
