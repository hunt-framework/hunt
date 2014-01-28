{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeFamilies      #-}

module Holumbus.IndexHandler where

import           Control.Monad

import           Data.Set                          (Set)
import qualified Data.Set                          as S
import qualified Data.IntSet                       as IS
import           Data.Map                          (Map)
import qualified Data.Map                          as M
import           Data.Maybe
import           Data.Binary                       (Binary (..))
import           Data.Binary.Get
import           Data.ByteString.Lazy              (ByteString)

import           Holumbus.DocTable.DocTable        (DocTable)
import qualified Holumbus.DocTable.DocTable        as Dt

import           Holumbus.Common
import           Holumbus.Common.DocIdMap          (toDocIdSet)
import qualified Holumbus.Common.Document          as Doc
import qualified Holumbus.Common.Occurrences       as Occ

import qualified Holumbus.Index.Proxy.ContextIndex as CIx
import           Holumbus.Index.Proxy.ContextIndex (ContextIndex)
import           Holumbus.Index.IndexImpl          (IndexImpl)

import           Holumbus.Utility

-- ----------------------------------------------------------------------------

data IndexHandler dt = IXH
  { ixhIndex  :: CIx.ContextIndex Occurrences
  , ixhDocs   :: dt
  , ixhSchema :: Schema
  }

-- ----------------------------------------------------------------------------
decodeIXH :: (Binary dt, DocTable dt) => [IndexImpl Occurrences] -> ByteString -> IndexHandler dt
decodeIXH ts = runGet (get' ts)

get' :: Binary dt => [IndexImpl Occurrences] -> Get (IndexHandler dt)
get' ts = liftM3 IXH (CIx.get' ts) get get

instance Binary dt => Binary (IndexHandler dt) where
  get = liftM3 IXH get get get
  put (IXH a b c) = put a >> put b >> put c

-- ----------------------------------------------------------------------------

-- | Insert a Document and Words.
insert :: (Monad m, DocTable dt)
       => Dt.DValue dt -> Words -> IndexHandler dt -> m (IndexHandler dt)
insert doc wrds (IXH ix dt s) = do
    (did, newDt) <- Dt.insert dt doc
    newIx        <- addWords wrds did ix
    return $ IXH newIx newDt s

-- | Update elements
update :: (Monad m, DocTable dt)
       => DocId -> Dt.DValue dt -> Words
       -> IndexHandler dt -> m (IndexHandler dt)
update docId doc' w ix = do
    ix' <- delete ix (IS.singleton docId)
    insert doc' w ix'

-- | Modify elements
modify :: (Monad m, DocTable dt)
       => (Dt.DValue dt -> m (Dt.DValue dt))
       -> Words -> DocId -> IndexHandler dt -> m (IndexHandler dt)
modify f wrds dId (IXH ii dt s) = do
  newDocTable <- Dt.adjust f dId dt
  newIndex    <- addWords wrds dId ii
  return $ IXH newIndex newDocTable s

-- | Delete a set of documents by 'URI'.
deleteDocsByURI :: (Monad m, DocTable dt)
                => Set URI -> IndexHandler dt -> m (IndexHandler dt)
deleteDocsByURI us ixx@(IXH _ix dt _) = do
    docIds <- liftM (toDocIdSet . catMaybes) . mapM (Dt.lookupByURI dt) . S.toList $ us
    delete ixx docIds

-- | Delete a set of documents by 'DocId'.
delete :: (Monad m, DocTable dt)
       => IndexHandler dt -> DocIdSet -> m (IndexHandler dt)
delete (IXH ix dt s) dIds = do
    newIx <- CIx.delete dIds ix
    newDt <- Dt.difference dIds dt
    return $ IXH newIx newDt s

-- | All contexts.
contexts :: (Monad m, DocTable dt)
         => IndexHandler dt -> m [Context]
contexts (IXH ix _dt _s) = return $ CIx.contexts ix

-- | Does the context exist?
hasContext :: (Monad m, DocTable dt)
           => Context -> IndexHandler dt -> m Bool
hasContext c (IXH ix _dt _s) = return $ CIx.hasContext c ix

-- | Is the document part of the index?
member :: (Monad m, DocTable dt)
       => URI -> IndexHandler dt -> m Bool
member u (IXH _ii dt _s) = do
  mem <- Dt.lookupByURI dt u
  return $ isJust mem
-- ----------------------------------------------------------------------------

-- | Modify the description of a document and add words
--   (occurrences for that document) to the index.
modifyWithDescription :: (Monad m, DocTable dt)
                      => Description -> Words -> DocId -> IndexHandler dt -> m (IndexHandler dt)
modifyWithDescription descr wrds dId (IXH ii dt s) = do
    newDocTable <- Dt.adjust mergeDescr dId dt
    newIndex    <- addWords wrds dId ii
    return $ IXH newIndex newDocTable s
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
addWords :: Monad m => Words -> DocId -> ContextIndex Occurrences -> m (ContextIndex Occurrences)
addWords wrds dId i
  = foldrWithKeyM (\c wl acc ->
      foldrWithKeyM (\w ps acc' ->
        CIx.insertWithCx c w (mkOccs dId ps) acc')
      acc wl)
      i wrds
  where
  mkOccs            :: DocId -> [Position] -> Occurrences
  mkOccs did pl     = positionsIntoOccs did pl Occ.empty

  positionsIntoOccs :: DocId -> [Position] -> Occurrences -> Occurrences
  positionsIntoOccs docId ws os = foldr (Occ.insert docId) os ws
