{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE Rank2Types        #-}

module Holumbus.Indexer.TextIndexer where

import           Data.Set                          (Set)
import qualified Data.Set                          as S
import qualified Data.IntSet                       as IS
import qualified Data.Map                          as M

import           Holumbus.Utility                  (catMaybesSet)

import           Holumbus.DocTable.DocTable        (DocTable)
import qualified Holumbus.DocTable.DocTable        as Dt

import           Holumbus.Common                   hiding (delete)
import           Holumbus.Common.DocIdMap          (toDocIdSet)

import qualified Holumbus.Index.Index              as Ix
import qualified Holumbus.Index.TextIndex          as TIx
import qualified Holumbus.Index.Proxy.ContextIndex as CIx

import           Holumbus.Indexer.Indexer
-- ----------------------------------------------------------------------------

type TextIndexerCon i dt
    = (TIx.TextIndex i Occurrences, DocTable dt, Dt.DValue dt ~ Document)

type TextIndexer i dt = Indexer i Occurrences dt
type ContextTextIndexer i dt = ContextIndexer i Occurrences dt

-- ----------------------------------------------------------------------------

-- | Insert a Document and Words.
insert :: TextIndexerCon i dt
       => (Dt.DValue dt) -> Words -> ContextTextIndexer i dt -> ContextTextIndexer i dt
insert doc wrds (ix,dt)
    = (newIx, newDt)
    where
    (did, newDt) = Dt.insert dt doc
    newIx        = TIx.addWords wrds did ix

-- | Update elements
update :: TextIndexerCon i dt 
       => DocId -> Dt.DValue dt -> Words 
       -> ContextTextIndexer i dt -> ContextTextIndexer i dt
update docId doc' w ix
  = insert doc' w ix'
  where
  ix' = delete ix (IS.singleton docId)

-- | Modify elements
modify :: (TextIndexerCon i dt)
       => (Dt.DValue dt -> Dt.DValue dt)
       -> Words -> DocId -> ContextTextIndexer i dt -> ContextTextIndexer i dt
modify f wrds dId (ii,dt)
  = (newIndex,newDocTable)
  where
  newDocTable = Dt.adjust f dId dt
  newIndex    = TIx.addWords wrds dId ii


-- | Delete a set if documents by 'URI'.
deleteDocsByURI :: TextIndexerCon i dt 
                => Set URI -> ContextTextIndexer i dt -> ContextTextIndexer i dt
deleteDocsByURI us ixx@(_ix,dt)
    = delete ixx docIds
    where
    docIds = toDocIdSet . catMaybesSet . S.map (Dt.lookupByURI dt) $ us

-- | Delete a set of documents by 'DocId'.
delete :: TextIndexerCon i dt => ContextTextIndexer i dt -> DocIdSet -> ContextTextIndexer i dt
delete (ix,dt) dIds
  = (newIx, newDt)
    where
    newIx = CIx.map (Ix.batchDelete dIds) ix
    newDt = Dt.difference dIds            dt

-- ----------------------------------------------------------------------------

-- | Modify the description of a document and add words
--   (occurrences for that document) to the index.
modifyWithDescription :: (TextIndexerCon i dt)
                      => Description -> Words -> DocId 
                      -> ContextTextIndexer i dt -> ContextTextIndexer i dt
modifyWithDescription descr wrds dId (ii,dt)
  = (newIndex, newDocTable)
  where
  newDocTable = Dt.adjust mergeDescr dId dt
  newIndex    = TIx.addWords wrds dId ii
  -- M.union is left-biased - flip to use new values for existing keys - no flip to keep old values
  mergeDescr d = d{ desc = flip M.union (desc d) descr }

-- ----------------------------------------------------------------------------

-- Helper functions
-- Specific to Indexes with Document DocTable values
{-
addDocDescription         :: Description -> DocId -> Indexer it iv i d Document -> Indexer it iv i d Document
addDocDescription descr did (Indexer i d)
  = Indexer i (Dt.modify mergeDescr did d)
  where
  mergeDescr doc = doc{ desc = M.union (desc doc) descr }
-}
----------------------------------------------------------------------------
