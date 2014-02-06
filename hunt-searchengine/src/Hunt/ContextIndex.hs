{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Hunt.ContextIndex where

import           Prelude
import qualified Prelude                           as P

import           Control.Arrow
import           Control.Monad
import qualified Control.Monad.Parallel            as Par

import           Data.Set                          (Set)
import qualified Data.Set                          as S
import qualified Data.IntSet                       as IS
import qualified Data.Map                          as M
import           Data.Map                          (Map)
import           Data.Maybe
import           Data.Binary                       (Binary (..))
import           Data.Binary.Get
import           Data.ByteString.Lazy              (ByteString)
import           Data.Text                         (Text)
import qualified Data.Traversable                  as TV

import           Hunt.DocTable.DocTable            (DocTable)
import qualified Hunt.DocTable.DocTable            as Dt

import           Hunt.Common
import           Hunt.Common.DocIdMap              (toDocIdSet)
import qualified Hunt.Common.Document              as Doc
import qualified Hunt.Common.Occurrences           as Occ

import qualified Hunt.Index.Index                  as Ix
import           Hunt.Index.IndexImpl              (IndexImpl)
import qualified Hunt.Index.IndexImpl              as Impl

import           Hunt.Utility

-- ----------------------------------------------------------------------------

data ContextIndex dt = ContextIx
  { ixhIndex  :: !(ContextMap Occurrences)
  , ixhDocs   :: dt
  , ixhSchema :: Schema
  }

newtype ContextMap v
  = ContextMap { cxMap :: Map Context (Impl.IndexImpl v) }
  deriving (Show)

-- ----------------------------------------------------------------------------

getContextMap :: [IndexImpl Occurrences] -> Get (ContextMap Occurrences)
getContextMap ts = liftM M.fromDistinctAscList (Impl.get' ts) >>= return . ContextMap

instance Binary v => Binary (ContextMap v) where
  put = put . cxMap
  get = get >>= return . ContextMap

-- ----------------------------------------------------------------------------

decodeCxIx :: (Binary dt, DocTable dt) => [IndexImpl Occurrences] -> ByteString -> ContextIndex dt
decodeCxIx ts = runGet (get' ts)

get' :: Binary dt => [IndexImpl Occurrences] -> Get (ContextIndex dt)
get' ts = liftM3 ContextIx (getContextMap ts) get get

instance Binary dt => Binary (ContextIndex dt) where
  get = liftM3 ContextIx get get get
  put (ContextIx a b c) = put a >> put b >> put c

-- ----------------------------------------------------------------------------

-- | Insert a Document and Words.
--   /NOTE/: For multiple inserts, use the more efficient 'batchInsert'.
insert :: (Par.MonadParallel m, DocTable dt)
       => Dt.DValue dt -> Words -> ContextIndex dt -> m (ContextIndex dt)
insert doc wrds (ContextIx ix dt s) = do
    (did, newDt) <- Dt.insert dt doc
    newIx        <- addWords wrds did ix
    return $ ContextIx newIx newDt s

-- | Insert multiple Documents and Words.
--   This is more efficient than using fold and 'insert'.
batchInsert :: (Par.MonadParallel m, DocTable dt)
       => [(Dt.DValue dt, Words)] -> ContextIndex dt -> m (ContextIndex dt)
batchInsert docAndWrds (ContextIx ix dt s) = do
    (newDt, docIdsAndWrds)
        <- foldM'
              (\(docTable, wordAcc) (doc, wrds)
                  -> Dt.insert docTable doc
                      >>= \(dId, docTable') -> return (docTable', (dId, wrds):wordAcc))
              (dt, [])
              docAndWrds
    newIx <- batchAddWords docIdsAndWrds ix
    return $ ContextIx newIx newDt s

-- | Update elements
update :: (Par.MonadParallel m, DocTable dt)
       => DocId -> Dt.DValue dt -> Words
       -> ContextIndex dt -> m (ContextIndex dt)
update docId doc' w ix = do
    ix' <- delete ix (IS.singleton docId)
    insert doc' w ix'

-- | Modify elements
modify :: (Par.MonadParallel m, DocTable dt)
       => (Dt.DValue dt -> m (Dt.DValue dt))
       -> Words -> DocId -> ContextIndex dt -> m (ContextIndex dt)
modify f wrds dId (ContextIx ii dt s) = do
  newDocTable <- Dt.adjust f dId dt
  newIndex    <- addWords wrds dId ii
  return $ ContextIx newIndex newDocTable s

-- | Delete a set of documents by 'URI'.
deleteDocsByURI :: (Monad m, DocTable dt)
                => Set URI -> ContextIndex dt -> m (ContextIndex dt)
deleteDocsByURI us ixx@(ContextIx _ix dt _) = do
    docIds <- liftM (toDocIdSet . catMaybes) . mapM (Dt.lookupByURI dt) . S.toList $ us
    delete ixx docIds

-- | Delete a set of documents by 'DocId'.
delete :: (Monad m, DocTable dt)
       => ContextIndex dt -> DocIdSet -> m (ContextIndex dt)
delete (ContextIx ix dt s) dIds = do
    newIx <- delete' dIds ix
    newDt <- Dt.difference dIds dt
    return $ ContextIx newIx newDt s

-- | All contexts.
contexts :: (Monad m, DocTable dt)
         => ContextIndex dt -> m [Context]
contexts (ContextIx ix _dt _s) = return $ contexts' ix

-- | Does the context exist?
hasContext :: (Monad m, DocTable dt)
           => Context -> ContextIndex dt -> m Bool
hasContext c (ContextIx ix _dt _s) = return $ hasContext' c ix

-- | Is the document part of the index?
member :: (Monad m, DocTable dt)
       => URI -> ContextIndex dt -> m Bool
member u (ContextIx _ii dt _s) = do
  mem <- Dt.lookupByURI dt u
  return $ isJust mem
-- ----------------------------------------------------------------------------

-- | Modify the description of a document and add words
--   (occurrences for that document) to the index.
modifyWithDescription :: (Par.MonadParallel m, DocTable dt)
                      => Description -> Words -> DocId -> ContextIndex dt -> m (ContextIndex dt)
modifyWithDescription descr wrds dId (ContextIx ii dt s) = do
    newDocTable <- Dt.adjust mergeDescr dId dt
    newIndex    <- addWords wrds dId ii
    return $ ContextIx newIndex newDocTable s
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


addWords :: Par.MonadParallel m => Words -> DocId -> ContextMap Occurrences -> m (ContextMap Occurrences)
addWords wrds dId _i@(ContextMap m)
  = mapWithKeyMP (\cx impl -> foldInsert cx impl wrds dId) m >>= return . ContextMap
--  = foldrWithKeyM (\c wl acc ->
--      foldrWithKeyM (\w ps acc' ->
--        insertWithCx c w (mkOccs dId ps) acc')
--      acc wl) i wrds
  where
  foldInsert :: Monad m => Context -> IndexImpl Occurrences -> Words -> DocId -> m (IndexImpl Occurrences)
  foldInsert cx (Impl.IndexImpl impl) ws docId
    = Ix.batchInsert (map (second (mkOccs docId)) $ M.toList (getWlForCx cx ws)) impl >>= return . Impl.mkIndex


-- | Add words for a document to the 'Index'.
--   /NOTE/: adds words to /existing/ 'Context's.
batchAddWords :: Par.MonadParallel m => [(DocId, Words)] -> ContextMap Occurrences -> m (ContextMap Occurrences)
batchAddWords wrdsAndDocIds _i@(ContextMap m)
  = mapWithKeyMP (\cx impl -> foldBatchInsert cx impl wrdsAndDocIds) m >>= return . ContextMap
  where
  foldBatchInsert :: Monad m => Context -> IndexImpl Occurrences -> [(DocId, Words)] -> m (IndexImpl Occurrences)
  foldBatchInsert cx (Impl.IndexImpl impl) docIdsAndWrds
    = Ix.batchInsert (concat . map ((\(did, wl) -> map (second (mkOccs did)) $ M.toList wl) . second (getWlForCx cx)) $ docIdsAndWrds) impl
        >>= return . Impl.mkIndex

----------------------------------------------------------------------------
-- addWords/batchAddWords functions

positionsIntoOccs :: DocId -> [Position] -> Occurrences -> Occurrences
positionsIntoOccs docId ws os = foldr (Occ.insert docId) os ws

mkOccs            :: DocId -> [Position] -> Occurrences
mkOccs did pl     = positionsIntoOccs did pl Occ.empty

getWlForCx :: Context -> Words -> WordList
getWlForCx cx ws = fromMaybe M.empty (M.lookup cx ws)

mapWithKeyM :: (Monad m, Ord k) => (k -> a -> m b) -> M.Map k a -> m (M.Map k b)
mapWithKeyM f m =
  (P.mapM (\(k, a) -> do
                  b <- f k a
                  return (k, b)
                ) $ M.toList m) >>=
    return . M.fromList

-- | parallel map
--   increasing cpu usage from 30% to 60%
--   but runtime is actually slower
mapWithKeyMP :: (Par.MonadParallel m, Ord k) => (k -> a -> m b) -> M.Map k a -> m (M.Map k b)
mapWithKeyMP f m =
  (Par.mapM (\(k, a) -> do
                  b <- f k a
                  return (k, b)
                ) $ M.toList m) >>=
    return . M.fromList

----------------------------------------------------------------------------

-- | Empty ContextMap.
empty :: ContextMap v
empty = ContextMap $ M.empty

-- | Insert a new context.
--   Note: If context already exists this function does nothing.
insertContext' :: Impl.IndexImplCon ix v
              => Context -> ix v -> ContextMap v -> ContextMap v
insertContext' c ix (ContextMap m) = ContextMap $ M.insertWith (const id) c (Impl.mkIndex ix) m

insertContext :: Context -> Impl.IndexImpl v -> ContextMap v -> ContextMap v
insertContext c ix (ContextMap m) = ContextMap $ M.insertWith (const id) c ix m


-- | Removes context including attached index from ContextMap.
deleteContext :: Context -> ContextMap v -> ContextMap v
deleteContext c (ContextMap m) = ContextMap $ M.delete c m

-- | Insert an element to one Context.
insertWithCx :: Monad m => Context -> Text -> v -> ContextMap v -> m (ContextMap v)
insertWithCx c w v (ContextMap m)
  = case M.lookup c m of
      Just (Impl.IndexImpl ix) -> do
        ix' <- liftM Impl.mkIndex $ Ix.batchInsert [(w,v)] ix
        return $ ContextMap $ M.insert c ix' m
      _      -> error "context does not exist"
  --where
  --adjust' (Impl.IndexImpl ix) = Impl.mkIndex $

delete' :: Monad m => DocIdSet -> ContextMap v -> m (ContextMap v)
delete' dIds (ContextMap m)
  = liftM ContextMap $ TV.mapM adjust' m
  where
  adjust' (Impl.IndexImpl ix) = liftM Impl.mkIndex $ Ix.batchDelete dIds ix

search :: Monad m => TextSearchOp -> Text -> ContextMap v -> m [(Context, [(Text, v)])]
search op k (ContextMap m)
  = liftM M.toList $ TV.mapM search' m
  where
  search' (Impl.IndexImpl ix) = Ix.search op k ix

-- XXX: code duplication? - see searchwithcx...
lookupRangeCx :: Monad m => Context -> Text -> Text -> ContextMap v -> m [(Text, v)]
lookupRangeCx c k1 k2 (ContextMap m)
  = case M.lookup c m of
      Just (Impl.IndexImpl cm) -> Ix.lookupRange k1 k2 cm
      _                        -> return []

lookupRangeCxs :: Monad m => [Context] -> Text -> Text -> ContextMap v -> m [(Context, [(Text, v)])]
lookupRangeCxs cs k1 k2 (ContextMap m)
  = P.mapM search' cs
  where
  search' c = case M.lookup c m of
      Just (Impl.IndexImpl cm) -> do
        ix <- Ix.lookupRange k1 k2 cm
        return (c, ix)
      _ -> return (c, [])

searchWithCx :: Monad m => TextSearchOp -> Context -> Text -> ContextMap v -> m [(Text, v)]
searchWithCx op c k (ContextMap m)
  = case M.lookup c m of
      Just (Impl.IndexImpl cm) -> Ix.search op k cm
      _                        -> return []

-- | XXX we actually do not have any parallelism here at the moment
--   because everything is evalutated lazy!
searchWithCxs :: Monad m => TextSearchOp -> [Context] -> Text -> ContextMap v -> m [(Context, [(Text, v)])]
searchWithCxs op cs k (ContextMap m)
  = P.mapM search' cs
  where
  search' c = case M.lookup c m of
      Just (Impl.IndexImpl cm) -> do
        ix <- Ix.search op k cm
        return (c, ix)
      _ -> return (c, [])

-- | search in different contexts with key already normalized in respect to each context type
searchWithCxsNormalized :: Monad m => TextSearchOp -> [(Context, Text)] -> ContextMap v -> m [(Context, [(Text, v)])]
searchWithCxsNormalized op cks (ContextMap m)
  = P.mapM search' cks
  where
  search' (c, k) = case M.lookup c m of
      Just (Impl.IndexImpl cm) -> do
        ix <- Ix.search op k cm
        return (c, ix)
      _ -> return (c, [])

-- ----------------------------------------------------------------------------

-- | Contexts/keys of 'ContextMap'.
contexts' :: ContextMap v -> [Context]
contexts' (ContextMap m) = M.keys m

-- | Check if the context exists.
hasContext' :: Context -> ContextMap v -> Bool
hasContext' c (ContextMap m) = M.member c m
