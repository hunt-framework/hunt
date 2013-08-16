{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Holumbus.Index.Common.DocId
where

import Control.DeepSeq

import Data.Binary              ( Binary (..) )
import qualified
       Data.Binary              as B
import Data.Int                ( Int64 )

-- ------------------------------------------------------------

-- | The unique identifier of a document
-- (created upon insertion into the document table).
newtype DocId                   = DocId { theDocId :: Int64 }
                                  deriving (Eq, Ord, Enum, NFData)

-- | Show instance for 'DocId'.
instance Show DocId where
    show                        = show . theDocId
    {-# INLINE show #-}

-- | Binary serialisation for 'DocId'.
instance Binary DocId where
    put                         = B.put . theDocId
    get                         = B.get >>= return . DocId
    {-# INLINE put #-}
    {-# INLINE get #-}

-- | Increase the 'DocId' by 1.
incrDocId                       :: DocId -> DocId
incrDocId                       = DocId . (1+) . theDocId

-- | Add two 'DocId's.
addDocId                        :: DocId -> DocId -> DocId
addDocId id1 id2                = DocId $ theDocId id1 + theDocId id2

-- | Subtract two 'DocId's.
subDocId                        :: DocId -> DocId -> DocId
subDocId id1 id2                = DocId $ theDocId id1 - theDocId id2

-- | DocId 0.
nullDocId                       :: DocId
nullDocId                       = DocId 0

-- | DocId 1.
firstDocId                      :: DocId
firstDocId                      = DocId 1

-- XXX: Integer to Int64?
-- | Create a 'DocId' from an 'Integer'.
mkDocId                         :: Integer -> DocId
mkDocId                         = DocId . fromIntegral


{-# INLINE incrDocId #-}
{-# INLINE addDocId #-}
{-# INLINE subDocId #-}
{-# INLINE nullDocId #-}
{-# INLINE firstDocId #-}
{-# INLINE mkDocId #-}

-- ------------------------------------------------------------
