-- ----------------------------------------------------------------------------

{- |
  Module     : Hunt.Index.Common.DocId
  Copyright  : Copyright (C) 2011 Sebastian M. Schlatt, Timo B. Huebel, Uwe Schmidt
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: none portable

  The document identifier type DocId

-}

-- ----------------------------------------------------------------------------

module Hunt.Common.DocId
where

-- ----------------------------------------------------------------------------

-- | The unique identifier of a document.
type DocId = Int

-- ----------------------------------------------------------------------------

mkNull :: DocId
mkNull = 0

mkFirst :: DocId
mkFirst = 1

fromInteger :: Integer -> DocId
fromInteger = fromIntegral

-- ----------------------------------------------------------------------------

{-# INLINE mkNull #-}
{-# INLINE mkFirst #-}
{-# INLINE fromInteger #-}

-- ----------------------------------------------------------------------------
