{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE Rank2Types        #-}

module Holumbus.IndexHandler where

import           Control.Monad

import           Data.Set                          (Set)
import qualified Data.Set                          as S
import qualified Data.IntSet                       as IS
import qualified Data.Map                          as M
import           Data.Maybe

import           Holumbus.DocTable.DocTable        (DocTable)
import qualified Holumbus.DocTable.DocTable        as Dt

import           Holumbus.Common
import           Holumbus.Common.DocIdMap          (toDocIdSet)
import qualified Holumbus.Common.Document          as Doc
import qualified Holumbus.Common.Occurrences       as Occ

import qualified Holumbus.Index.Proxy.ContextIndex as CIx
import           Holumbus.Index.Proxy.ContextIndex (ContextIndex)

-- ----------------------------------------------------------------------------

type IndexHandler   dt = (CIx.ContextIndex Occurrences, dt, Schema)

-- ----------------------------------------------------------------------------

-- | Insert a Document and Words.
insert :: (Monad m, DocTable dt)
       => (Dt.DValue dt) -> Words -> IndexHandler dt -> m (IndexHandler dt)
insert doc wrds (ix, dt, s) = do
    (did, newDt) <- Dt.insert dt doc
    let newIx = addWords wrds did ix
    return (newIx, newDt, s)

-- | Update elements
update :: (Monad m,DocTable dt)
       => DocId -> Dt.DValue dt -> Words
       -> IndexHandler dt -> m (IndexHandler dt)
update docId doc' w ix = do
    ix' <- delete ix (IS.singleton docId)
    insert doc' w ix'


-- | Modify elements
modify :: (Monad m,DocTable dt)
       => (Dt.DValue dt -> m (Dt.DValue dt))
       -> Words -> DocId -> IndexHandler dt -> m (IndexHandler dt)
modify f wrds dId (ii, dt, s) = do
  newDocTable <- Dt.adjust f dId dt
  let newIndex = addWords wrds dId ii
  return (newIndex, newDocTable, s)

-- | Delete a set of documents by 'URI'.
deleteDocsByURI :: (Monad m,DocTable dt)
                => Set URI -> IndexHandler dt -> m (IndexHandler dt)
deleteDocsByURI us ixx@(_ix,dt,_) = do
    docIds <- liftM (toDocIdSet . catMaybes) . mapM (Dt.lookupByURI dt) . S.toList $ us
    delete ixx docIds

-- | Delete a set of documents by 'DocId'.
delete :: (Monad m,DocTable dt)
       => IndexHandler dt -> DocIdSet -> m (IndexHandler dt)
delete (ix,dt,s) dIds = do
    let newIx = CIx.delete dIds ix
    newDt <- Dt.difference dIds dt
    return (newIx, newDt, s)

-- | All contexts.
contexts :: (Monad m,DocTable dt)
         => IndexHandler dt -> m [Context]
contexts (ix,_dt,_s) = return $ CIx.contexts ix

-- | Does the context exist?
hasContext :: (Monad m,DocTable dt)
           => Context -> IndexHandler dt -> m Bool
hasContext c (ix,_dt,_s) = return $ CIx.hasContext c ix

-- | Is the document part of the index?
member :: (Monad m,DocTable dt)
       => URI -> IndexHandler dt -> m Bool
member u (_ii, dt, _s) = do
  mem <- Dt.lookupByURI dt u
  return $ isJust mem
-- ----------------------------------------------------------------------------

-- | Modify the description of a document and add words
--   (occurrences for that document) to the index.
modifyWithDescription :: (Monad m,DocTable dt)
                      => Description -> Words -> DocId
                      -> IndexHandler dt -> m (IndexHandler dt)
modifyWithDescription descr wrds dId (ii, dt, s) = do
    newDocTable <- Dt.adjust mergeDescr dId dt
    let newIndex = addWords wrds dId ii
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
-- helper

-- | Add words for a document to the 'Index'.
--   /NOTE/: adds words to /existing/ 'Context's.
addWords :: Words -> DocId -> ContextIndex Occurrences -> ContextIndex Occurrences
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
