{-# OPTIONS -fno-warn-orphans #-}

module GHC.Fingerprint.Binary where

import           Control.Applicative

import           Data.Binary

import           GHC.Fingerprint.Type

-- ----------------------------------------------------------------------------

instance Binary Fingerprint where
  put (Fingerprint hi lo) = put hi >> put lo
  get = Fingerprint <$> get <*> get

-- ----------------------------------------------------------------------------
