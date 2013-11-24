{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeFamilies      #-}

module Holumbus.Index.TextIndex
( TextIndex
, ContextTextIndex
, addWords
)
where

import qualified Data.Map                          as M

import qualified Holumbus.Common.Occurrences       as Occ

import           Holumbus.Common

import           Holumbus.Index.Index
import           Holumbus.Index.Proxy.ContextIndex (ContextIndex, ContextIxCon)
import qualified Holumbus.Index.Proxy.ContextIndex as CIx

-- ----------------------------------------------------------------------------

-- Requires 'ConstraintKinds' extension
type TextIndex i v
  = ( Index i
    , ICon i v
    , v ~ IVal i v
    , v ~ Occurrences
    , IKey i v ~ Word
    , ISearchOp i v ~ TextSearchOp
    )

type ContextTextIndex i v
  = ( ContextIxCon i v
    , Index i
    , ICon i Occurrences
    , IKey i Occurrences ~ Word
    , TextIndex i v
    )

-- ----------------------------------------------------------------------------

-- | Add words for a document to the 'Index'.
--   /NOTE/: adds words to /existing/ 'Context's.
addWords :: TextIndex i Occurrences
         => Words -> DocId -> ContextIndex i Occurrences -> ContextIndex i Occurrences
addWords wrds dId i
  = M.foldrWithKey (\c wl acc ->
      M.foldrWithKey (\w ps acc' ->
        CIx.insertWithCx c w (mkOccs dId ps) acc')
      acc wl)
      i wrds
  where
  mkOccs            :: DocId -> [Position] -> Occurrences
  mkOccs did pl     = positionsIntoOccs did pl Occ.empty

  positionsIntoOccs :: DocId -> [Position] -> Occurrences -> Occurrences
  positionsIntoOccs docId ws os = foldr (Occ.insert docId) os ws
