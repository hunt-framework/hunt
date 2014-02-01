{-# OPTIONS -fno-warn-orphans #-}

-- ----------------------------------------------------------------------------

module Data.Text.Binary
where

import           Control.Monad      (liftM)
import           Data.Binary        (Binary (..))
import           Data.Text
import           Data.Text.Encoding as TE

instance Binary Text where
  put = put . encodeUtf8
  get = liftM decodeUtf8 get

-- ----------------------------------------------------------------------------
