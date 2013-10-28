-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Common.DocId
  Copyright  : Copyright (C) 2011 Sebastian M. Schlatt, Timo B. Huebel, Uwe Schmidt
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: none portable

  The document identifier type DocId

-}

-- ----------------------------------------------------------------------------

module Holumbus.Common.DocId
where

-- ------------------------------------------------------------

-- | The unique identifier of a document
-- (created upon insertion into the document table).

type DocId                   = Int

incrDocId                       :: DocId -> DocId
incrDocId                       = (1+)

addDocId                        :: DocId -> DocId -> DocId
addDocId                        = (+)

subDocId                        :: DocId -> DocId -> DocId
subDocId                        = (-)

nullDocId                       :: DocId
nullDocId                       = 0

firstDocId                      :: DocId
firstDocId                      = 1

mkDocId                         :: Integer -> DocId
mkDocId                         = fromIntegral

{-# INLINE incrDocId #-}
{-# INLINE addDocId #-}
{-# INLINE subDocId #-}
{-# INLINE nullDocId #-}
{-# INLINE firstDocId #-}
{-# INLINE mkDocId #-}

-- ------------------------------------------------------------
