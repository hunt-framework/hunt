{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Offset where

import qualified Foreign.Storable as Storable

-- | Offset with a type annotation, useful for not mixing up
-- offsets into different files.
newtype OffsetOf a
  = OffsetOf Int
  deriving (Eq, Ord, Show, Storable.Storable)
