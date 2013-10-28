{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE Rank2Types        #-}

module Holumbus.Index.TextIndex
( TextIndex
, ContextTextIndex
, addWords
)
where

import qualified Data.Map                            as M

import           Holumbus.Index.Common
import           Holumbus.Index.Index
import           Holumbus.Index.Proxy.ContextIndex   (ContextIxCon, ContextIndex)
import qualified Holumbus.Index.Proxy.ContextIndex   as CIx
import qualified Holumbus.Common.Occurrences         as Occ

-- ----------------------------------------------------------------------------

-- Requires 'ConstraintKinds' extension
type TextIndex i v
  = ( Index i
    , ICon i v
    , v ~ IVal i v
    , v ~ Occurrences
    , IKey i v ~ Word
    )

type ContextTextIndex i v = ( ContextIxCon i v
                            , Index i
                            , ICon i Occurrences
                            , IKey i Occurrences ~ Word
                            , TextIndex i v
                            )

-- | Add words for a document to the 'Index'.
-- | Note: adds words to every >existing< Context
addWords :: TextIndex i Occurrences 
         => Words -> DocId -> ContextIndex i Occurrences -> ContextIndex i Occurrences
addWords wrds dId i 
  = M.foldrWithKey (\c wl acc ->
      M.foldrWithKey (\w ps acc' ->
        CIx.insert (Just c, Just w) (mkOccs dId ps) acc')
      acc wl)
      i wrds
  where
  mkOccs            :: DocId -> [Position] -> Occurrences
  mkOccs did pl     = positionsIntoOccs did pl Occ.empty

  positionsIntoOccs :: DocId -> [Position] -> Occurrences -> Occurrences
  positionsIntoOccs docId ws os = foldr (Occ.insert docId) os ws

