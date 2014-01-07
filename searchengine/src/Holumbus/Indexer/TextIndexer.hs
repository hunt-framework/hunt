{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeFamilies      #-}

module Holumbus.Indexer.TextIndexer where

import           Control.Monad

import qualified Data.Binary                       as Bin
import qualified Data.IntSet                       as IS
import qualified Data.Map                          as M
import           Data.Maybe
import           Data.Set                          (Set)
import qualified Data.Set                          as S

import           Holumbus.DocTable.DocTable        (DocTable)
import qualified Holumbus.DocTable.DocTable        as Dt

import           Holumbus.Common
import           Holumbus.Common.DocIdMap          (toDocIdSet)
import qualified Holumbus.Common.Document          as Doc

import qualified Holumbus.Index.Proxy.ContextIndex as CIx
import qualified Holumbus.Index.TextIndex          as TIx

import           Holumbus.Indexer.Indexer

-- ----------------------------------------------------------------------------

type TextIndexerCon dt
    = (
        DocTable dt
      -- we need this for load and store, but i dont like these constraints here
      , Bin.Binary dt
      )

type TextIndexer        i dt = Indexer        i Occurrences dt
type ContextTextIndexer   dt = ContextIndexer   Occurrences dt

-- ----------------------------------------------------------------------------

-- | Insert a Document and Words.
insert :: (Monad m, TextIndexerCon dt)
       => (Dt.DValue dt) -> Words -> ContextTextIndexer dt -> m (ContextTextIndexer dt)
insert doc wrds (ix, dt, s) = do
    (did, newDt) <- Dt.insert dt doc
    let newIx = TIx.addWords wrds did ix
    return (newIx, newDt, s)

-- | Update elements
update :: (Monad m, TextIndexerCon dt)
       => DocId -> Dt.DValue dt -> Words
       -> ContextTextIndexer dt -> m (ContextTextIndexer dt)
update docId doc' w ix = do
    ix' <- delete ix (IS.singleton docId)
    insert doc' w ix'


-- | Modify elements
modify :: (Monad m, TextIndexerCon dt)
       => (Dt.DValue dt -> m (Dt.DValue dt))
       -> Words -> DocId -> ContextTextIndexer dt -> m (ContextTextIndexer dt)
modify f wrds dId (ii, dt, s) = do
  newDocTable <- Dt.adjust f dId dt
  let newIndex = TIx.addWords wrds dId ii
  return (newIndex, newDocTable, s)

-- | Delete a set of documents by 'URI'.
deleteDocsByURI :: (Monad m, TextIndexerCon dt)
                => Set URI -> ContextTextIndexer dt -> m (ContextTextIndexer dt)
deleteDocsByURI us ixx@(_ix,dt,_) = do
    docIds <- liftM (toDocIdSet . catMaybes) . mapM (Dt.lookupByURI dt) . S.toList $ us
    delete ixx docIds

-- | Delete a set of documents by 'DocId'.
delete :: (Monad m, TextIndexerCon dt)
       => ContextTextIndexer dt -> DocIdSet -> m (ContextTextIndexer dt)
delete (ix,dt,s) dIds = do
    let newIx = CIx.delete dIds ix
    newDt <- Dt.difference dIds dt
    return (newIx, newDt, s)

-- | All contexts.
contexts :: (Monad m, TextIndexerCon dt)
         => ContextTextIndexer dt -> m [Context]
contexts (ix,_dt,_s) = return $ CIx.contexts ix

-- | Does the context exist?
hasContext :: (Monad m, TextIndexerCon dt)
           => Context -> ContextTextIndexer dt -> m Bool
hasContext c (ix,_dt,_s) = return $ CIx.hasContext c ix

-- | Is the document part of the index?
member :: (Monad m, TextIndexerCon dt)
       => URI -> ContextTextIndexer dt -> m Bool
member u (_ii, dt, _s) = do
  mem <- Dt.lookupByURI dt u
  return $ isJust mem
-- ----------------------------------------------------------------------------

-- | Modify the description of a document and add words
--   (occurrences for that document) to the index.
modifyWithDescription :: (Monad m, TextIndexerCon dt)
                      => Description -> Words -> DocId
                      -> ContextTextIndexer dt -> m (ContextTextIndexer dt)
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
