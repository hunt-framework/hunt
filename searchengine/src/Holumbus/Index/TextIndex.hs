{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

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

-- ----------------------------------------------------------------------------

-- Requires 'ConstraintKinds' extension
type TextIndex i        = (Index i, IValue i ~ Occurrences, IType i ~ Textual)

-- requires 'FunctionalDependencies' in index type
-- | Insert a position for a single document.
insertPosition          :: TextIndex i => Context -> Word -> DocId -> Position -> i -> i
insertPosition c w d p  = insert c w (Occ.singleton d p)

-- | Delete a position for a single document.
deletePosition          :: TextIndex i => Context -> Word -> DocId -> Position -> i -> i
deletePosition c w d p  = delete c w (Occ.singleton d p)