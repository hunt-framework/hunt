{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE Rank2Types        #-}

module Holumbus.Indexer.TextIndexer where

import           Control.Monad

import           Data.Set                          (Set)
import qualified Data.Set                          as S
import qualified Data.IntSet                       as IS
import qualified Data.Map                          as M
import           Data.Maybe
import qualified Data.Binary                       as Bin

import           Holumbus.DocTable.DocTable        (DocTable)
import qualified Holumbus.DocTable.DocTable        as Dt

import           Holumbus.Common                   hiding (delete)
import           Holumbus.Common.DocIdMap          (toDocIdSet)
import qualified Holumbus.Common.Document          as Doc

import qualified Holumbus.Index.Index              as Ix
import qualified Holumbus.Index.TextIndex          as TIx
import qualified Holumbus.Index.Proxy.ContextIndex as CIx

import           Holumbus.Indexer.Indexer

-- ----------------------------------------------------------------------------

type TextIndexerCon i dt
    = ( TIx.TextIndex i Occurrences
      , DocTable dt
      -- we need this for load and store, but i dont like these constraints here
      , Bin.Binary dt
      , Bin.Binary (i Occurrences)
      )

type TextIndexer        i dt = Indexer        i Occurrences dt
type ContextTextIndexer i dt = ContextIndexer i Occurrences dt

-- ----------------------------------------------------------------------------

-- | Insert a Document and Words.
insert :: (Monad m, TextIndexerCon i dt)
       => (Dt.DValue dt) -> Words -> ContextTextIndexer i dt -> m (ContextTextIndexer i dt)
insert doc wrds (ix, dt, s) = do
    (did, newDt) <- Dt.insert dt doc
    let newIx = TIx.addWords wrds did ix
    return (newIx, newDt, s)

-- | Update elements
update :: (Monad m, TextIndexerCon i dt)
       => DocId -> Dt.DValue dt -> Words
       -> ContextTextIndexer i dt -> m (ContextTextIndexer i dt)
update docId doc' w ix = do
    ix' <- delete ix (IS.singleton docId)
    insert doc' w ix'


-- | Modify elements
modify :: (Monad m, TextIndexerCon i dt)
       => (Dt.DValue dt -> m (Dt.DValue dt))
       -> Words -> DocId -> ContextTextIndexer i dt -> m (ContextTextIndexer i dt)
modify f wrds dId (ii, dt, s) = do
  newDocTable <- Dt.adjust f dId dt
  let newIndex = TIx.addWords wrds dId ii
  return (newIndex, newDocTable, s)

-- | Delete a set of documents by 'URI'.
deleteDocsByURI :: (Monad m, TextIndexerCon i dt)
                => Set URI -> ContextTextIndexer i dt -> m (ContextTextIndexer i dt)
deleteDocsByURI us ixx@(_ix,dt,_) = do
    docIds <- liftM (toDocIdSet . catMaybes) . mapM (Dt.lookupByURI dt) . S.toList $ us
    delete ixx docIds

-- | Delete a set of documents by 'DocId'.
delete :: (Monad m, TextIndexerCon i dt)
       => ContextTextIndexer i dt -> DocIdSet -> m (ContextTextIndexer i dt)
delete (ix,dt,s) dIds = do
    let newIx = CIx.map (Ix.batchDelete dIds) ix
    newDt <- Dt.difference dIds dt
    return (newIx, newDt, s)

-- | All contexts.
contexts :: (Monad m, TextIndexerCon i dt)
         => ContextTextIndexer i dt -> m [Context]
contexts (ix,_dt,_s) = return $ CIx.contexts ix

-- | Does the context exist?
hasContext :: (Monad m, TextIndexerCon i dt)
           => Context -> ContextTextIndexer i dt -> m Bool
hasContext c (ix,_dt,_s) = return $ CIx.hasContext c ix

-- | Is the document part of the index?
member :: (Monad m, TextIndexerCon i dt)
       => URI -> ContextTextIndexer i dt -> m Bool
member u (_ii, dt, _s) = do
  mem <- Dt.lookupByURI dt u
  return $ isJust mem
-- ----------------------------------------------------------------------------

-- | Modify the description of a document and add words
--   (occurrences for that document) to the index.
modifyWithDescription :: (Monad m, TextIndexerCon i dt)
                      => Description -> Words -> DocId
                      -> ContextTextIndexer i dt -> m (ContextTextIndexer i dt)
modifyWithDescription descr wrds dId (ii, dt, s) = do
    newDocTable <- Dt.adjust mergeDescr dId dt
    let newIndex = TIx.addWords wrds dId ii
    return (newIndex, newDocTable, s)
    where
    -- M.union is left-biased - flip to use new values for existing keys - no flip to keep old values
    mergeDescr = return . Doc.update (\d' -> d'{ desc = flip M.union (desc d') descr })

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
