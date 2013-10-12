{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE Rank2Types        #-}

module Holumbus.Index.TextIndex
( module Holumbus.Index.Index
, TextIndex
, insertPosition
, deletePosition)
where

import           Holumbus.Index.Common             (Context, DocId, Occurrences,
                                                    Position, Textual, Word)
import qualified Holumbus.Index.Common.Occurrences as Occ
import           Holumbus.Index.Index
import           Holumbus.Index.Proxy.ContextIndex
import           Holumbus.Index.InvertedIndex

-- ----------------------------------------------------------------------------

-- Requires 'ConstraintKinds' extension
type TextIndex i v
  = ( Index i
    , ICon i v
    , v ~ IVal i v
    , v ~ Occurrences
    , IKey  i v ~ Word
    , IType i v ~ Textual
    , IToL  i v ~ [(Word, Occurrences)])

-- -- | Insert a position for a single document.
insertPosition c w d p i 
  = insert (Just c, Just w) (Occ.singleton d p) i

-- | xxx TODO implement delete
deletePosition c w d p i 
  = undefined
