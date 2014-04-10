{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- ----------------------------------------------------------------------------
{- |
  The context index introduces contexts and combines the index, document table and schema.
-}
-- ----------------------------------------------------------------------------

module Hunt.ContextIndex
  (
    -- * Construction
    empty

    -- * Contexts and Schema
  , insertContext
  , deleteContext
  , contexts
  , contexts'
  , hasContext'
  , hasContext

    -- * Queries
  , lookupRangeCx
  , searchWithCx
  , searchWithCxsNormalized

    -- * Insert\/Delete Documents
  , insertList
  , insertWithCx               -- only used in tests
  , modifyWithDescription
  , addWordsM                  -- only used in tests
  , delete
  , deleteDocsByURI
  , decodeCxIx
  , member

    -- * Types
  , ContextIndex (..)
  , ContextMap (..)
  )
where

import           Prelude
import qualified Prelude                     as P

import           Control.Applicative         (Applicative, (<$>), (<*>))
import           Control.Arrow
import           Control.Monad
import qualified Control.Monad.Parallel      as Par
--import           Control.Parallel.Strategies

import           Data.Binary                 (Binary (..))
import           Data.Binary.Get
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.IntSet                 as IS
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Maybe
import           Data.Set                    (Set)
import qualified Data.Set                    as S
import           Data.Text                   (Text)
import qualified Data.Traversable            as TV

import           Hunt.DocTable               (DocTable)
import qualified Hunt.DocTable               as Dt

import           Hunt.Common
import           Hunt.Common.DocIdMap        (toDocIdSet)
import qualified Hunt.Common.Document        as Doc
import qualified Hunt.Common.Occurrences     as Occ

import qualified Hunt.Index                  as Ix
import           Hunt.Index.IndexImpl        (IndexImpl)
import qualified Hunt.Index.IndexImpl        as Impl

import           Hunt.Utility

-- ------------------------------------------------------------

-- | Context index introduces contexts and combines the major components of Hunt.
data ContextIndex dt = ContextIndex
  { ciIndex  :: !(ContextMap Occurrences) -- ^ Indexes associated to contexts.
  , ciDocs   :: dt                        -- ^ Document table.
  , ciSchema :: Schema                    -- ^ Schema associated to contexts.
  }

-- | Contexts with associated heterogeneous index implementations.
newtype ContextMap v
  = ContextMap { cxMap :: Map Context (Impl.IndexImpl v) }
  deriving (Show)

-- | Strict smart constructor for the 'ContextMap'.
mkContextMap :: Map Context (Impl.IndexImpl v) -> ContextMap v
mkContextMap x = ContextMap $! x

-- ------------------------------------------------------------
-- Binary / Serialization
-- ------------------------------------------------------------

getContextMap :: [IndexImpl Occurrences] -> Get (ContextMap Occurrences)
getContextMap ts = liftM M.fromDistinctAscList (Impl.gets' ts) >>= return . mkContextMap

instance Binary v => Binary (ContextMap v) where
  put = put . cxMap
  get = get >>= return . mkContextMap


-- | Deserialize a 'ContextIndex' with the list of available index implementations.
--
--   /Note/: The serialized index implementations have to  be in the list of available types,
--           otherwise this will fail.
decodeCxIx :: (Binary dt, DocTable dt) => [IndexImpl Occurrences] -> ByteString -> ContextIndex dt
decodeCxIx ts = runGet (get' ts)

get' :: Binary dt => [IndexImpl Occurrences] -> Get (ContextIndex dt)
get' ts = ContextIndex <$> (getContextMap ts) <*> get <*> get

instance Binary dt => Binary (ContextIndex dt) where
  get = ContextIndex <$> get <*> get <*> get
  put (ContextIndex a b c) = put a >> put b >> put c

-- ------------------------------------------------------------

{-
-- | Insert a Document and Words.
--
--   /Note/: For multiple inserts, use the more efficient 'insertList'.
insert :: (Par.MonadParallel m, Applicative m, DocTable dt)
       => Dt.DValue dt -> Words -> ContextIndex dt -> m (ContextIndex dt)
insert doc wrds ix = insertList [(doc,wrds)] ix
-}

--   This is more efficient than using fold and with 'insert'.
-- | Insert multiple documents and words.
insertList :: (Par.MonadParallel m, Applicative m, DocTable dt)
       => [(Dt.DValue dt, Words)] -> ContextIndex dt -> m (ContextIndex dt)
insertList docAndWrds (ContextIndex ix docTable s) = do
  -- insert to doctable and generate docId
  tables <- Par.mapM subInsert $ partitionListByLength 20 docAndWrds
  (newDt, docIdsAndWrds) <- reduce tables docTable

  newIx <- batchAddWordsM docIdsAndWrds ix
  return $! ContextIndex newIx newDt s

  where
  subInsert ds = foldM (\(dt, withIds) (doc, wrds) -> do
                             (dId, dt') <- Dt.insert doc dt
                             return (dt', (dId, wrds):withIds)
                          ) (Dt.empty, []) ds

  reduce tables old = do
     step <- Par.mapM (\((dt1, ws1),(dt2, ws2)) -> Dt.union dt1 dt2 >>= \dt -> return (dt, ws1 ++ ws2)) $ mkPairs tables
     case step of
      []      -> return (Dt.empty, [])
      [(d,w)] -> Dt.union old d >>= \n -> return (n, w)
      xs      -> reduce xs old

  mkPairs []       = []
  mkPairs (a:[])   = [(a,(Dt.empty,[]))]
  mkPairs (a:b:xs) = (a,b):mkPairs xs


-- XXX: this should not work well atm
-- | Modify documents and index data.
modify :: (Par.MonadParallel m, Applicative m, DocTable dt)
       => (Dt.DValue dt -> m (Dt.DValue dt))
       -> Words -> DocId -> ContextIndex dt -> m (ContextIndex dt)
modify f wrds dId (ContextIndex ii dt s) = do
  newDocTable <- Dt.adjust f dId dt
  newIndex    <- addWordsM wrds dId ii
  return $ ContextIndex newIndex newDocTable s


-- | Delete a set of documents by 'URI'.
deleteDocsByURI :: (Par.MonadParallel m, Applicative m, DocTable dt)
                => Set URI -> ContextIndex dt -> m (ContextIndex dt)
deleteDocsByURI us ixx@(ContextIndex _ix dt _) = do
  docIds <- liftM (toDocIdSet . catMaybes) . mapM (flip Dt.lookupByURI dt) . S.toList $ us
  delete docIds ixx


-- | Delete a set of documents by 'DocId'.
delete :: (Par.MonadParallel m, Applicative m, DocTable dt)
       => DocIdSet -> ContextIndex dt -> m (ContextIndex dt)
delete dIds cix@(ContextIndex ix dt s)
    | IS.null dIds
        = return cix
    | otherwise
        = do newIx <- delete' dIds ix
             newDt <- Dt.difference dIds dt
             return $ ContextIndex newIx newDt s


-- | All contexts of the index.
contexts :: (Monad m, DocTable dt)
         => ContextIndex dt -> m [Context]
contexts (ContextIndex ix _dt _s) = return $ contexts' ix

-- | Does the context exist?
hasContext :: (Monad m, DocTable dt)
           => Context -> ContextIndex dt -> m Bool
hasContext c (ContextIndex ix _dt _s) = return $ hasContext' c ix


-- | Is the document part of the index?
member :: (Monad m, Applicative m, DocTable dt)
       => URI -> ContextIndex dt -> m Bool
member u (ContextIndex _ii dt _s) = do
  mem <- Dt.lookupByURI u dt
  return $ isJust mem
-- ------------------------------------------------------------

-- | Modify the description of a document and add words
--   (occurrences for that document) to the index.
modifyWithDescription :: (Par.MonadParallel m, Applicative m, DocTable dt)
                      => Maybe Float -> Description -> Words -> DocId -> ContextIndex dt -> m (ContextIndex dt)
modifyWithDescription weight descr wrds dId (ContextIndex ii dt s) = do
  newDocTable <- Dt.adjust mergeDescr dId dt
  newIndex    <- addWordsM wrds dId ii
  return $ ContextIndex newIndex newDocTable s
  where
  -- M.union is left-biased - flip to use new values for existing keys - no flip to keep old values
  mergeDescr = return . Doc.update (\d -> d{ desc = flip M.union (desc d) descr
                                           , wght = fromMaybe (wght d) weight })

-- ------------------------------------------------------------
-- Helper
-- ------------------------------------------------------------

-- | Adds words associated to a document to the index.
--
--   'insertList' and 'modifyWithDescription' are more convenient in most cases.
addWordsM :: Par.MonadParallel m => Words -> DocId -> ContextMap Occurrences -> m (ContextMap Occurrences)
addWordsM wrds dId (ContextMap m)
  = mapWithKeyMP (\cx impl -> foldInsert cx impl wrds dId) m >>= return . mkContextMap
  where
  foldInsert :: Monad m => Context -> IndexImpl Occurrences -> Words -> DocId -> m (IndexImpl Occurrences)
  foldInsert cx (Impl.IndexImpl impl) ws docId
    = Ix.insertListM (contentForCx cx [(docId,ws)]) impl >>= return . Impl.mkIndex

-- | Add words for a document to the 'Index'.
--
--   /Note/: Adds words to /existing/ 'Context's.
batchAddWordsM :: Par.MonadParallel m => [(DocId, Words)] -> ContextMap Occurrences -> m (ContextMap Occurrences)
batchAddWordsM vs (ContextMap m)
  = mapWithKeyMP (\cx impl -> foldinsertList cx impl) m >>= return . mkContextMap
  where
  foldinsertList :: Monad m => Context -> IndexImpl Occurrences -> m (IndexImpl Occurrences)
  foldinsertList cx (Impl.IndexImpl impl)
    = Ix.insertListM (contentForCx cx vs) impl >>= return . Impl.mkIndex

-- | Computes the words and occurrences out of a list for one context
contentForCx :: Content -> [(DocId, Words)] -> [(Word, Occurrences)]
contentForCx cx vs = (concat . map ((\(did, wl) -> map (second (mkOccs did)) $ M.toList wl) . second (getWlForCx cx)) $ vs)

-- | Add words for a document to the 'Index'.
--
--   /Note/: Adds words to /existing/ 'Context's.
{--
batchAddWords :: [(DocId, Words)] -> ContextMap Occurrences -> ContextMap Occurrences
batchAddWords vs (ContextMap m)
  = mkContextMap $ M.fromList $ parMap rpar (\(cx,impl) -> (cx,foldinsertList cx impl)) (M.toList m)
  where
  foldinsertList :: Context -> IndexImpl Occurrences-> IndexImpl Occurrences
  foldinsertList cx (Impl.IndexImpl impl)
    = Impl.mkIndex $ Ix.insertList (contentForCx cx vs) impl
--}
----------------------------------------------------------------------------
-- addWords/batchAddWords functions
----------------------------------------------------------------------------

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

mapWithKeyMP :: (Par.MonadParallel m, Ord k) => (k -> a -> m b) -> M.Map k a -> m (M.Map k b)
mapWithKeyMP f m =
  (Par.mapM (\(k, a) -> do
                  b <- f k a
                  return (k, b)
                ) $ M.toList m) >>=
    return . M.fromList

----------------------------------------------------------------------------

-- | Empty context map.
empty :: ContextMap v
empty = ContextMap $ M.empty

{-
-- | Inserts a new context.
--
--   /Note/: Does nothing if the context already exists.
insertContext' :: Impl.IndexImplCon ix v
              => Context -> ix v -> ContextMap v -> ContextMap v
insertContext' c ix (ContextMap m) = mkContextMap $ M.insertWith (const id) c (Impl.mkIndex ix) m
-}

-- | Inserts a new context.
--
--   /Note/: Does nothing if the context already exists.
insertContext :: Context -> Impl.IndexImpl v -> ContextMap v -> ContextMap v
insertContext c ix (ContextMap m) = mkContextMap $ M.insertWith (const id) c ix m



-- | Removes context (including the index and the schema).
deleteContext :: Context -> ContextIndex dt -> ContextIndex dt
deleteContext c (ContextIndex ix dt s) = ContextIndex (deleteContext' c ix) dt (M.delete c s)
  where
  -- | Removes context (includes the index, but not the schema).
  deleteContext' :: Context -> ContextMap v -> ContextMap v
  deleteContext' cx (ContextMap m) = mkContextMap $ M.delete cx m

-- | Insert an element to a 'Context'.
insertWithCx :: Monad m => Context -> Text -> v -> ContextMap v -> m (ContextMap v)
insertWithCx c w v (ContextMap m)
  = case M.lookup c m of
      Just (Impl.IndexImpl ix) -> do
        ix' <- liftM Impl.mkIndex $ Ix.insertListM [(w,v)] ix
        return $ ContextMap $ M.insert c ix' m
      _      -> error "context does not exist"

delete' :: Par.MonadParallel m => DocIdSet -> ContextMap v -> m (ContextMap v)
delete' dIds (ContextMap m)
  = mapWithKeyMP (\_ impl -> adjust' impl) m >>= return . mkContextMap
--  = TV.mapM adjust' m >>= return . mkContextMap
  where
  adjust' (Impl.IndexImpl ix) = liftM Impl.mkIndex $ Ix.deleteDocsM dIds ix

-- | Search query in all context.
search :: Monad m => TextSearchOp -> Text -> ContextMap v -> m [(Context, [(Text, v)])]
search op k (ContextMap m)
  = liftM M.toList $ TV.mapM search' m
  where
  search' (Impl.IndexImpl ix) = Ix.searchM op k ix

-- XXX: code duplication? - see searchwithcx...
-- | Range query in a context.
lookupRangeCx :: Monad m => Context -> Text -> Text -> ContextMap v -> m [(Text, v)]
lookupRangeCx c k1 k2 (ContextMap m)
  = case M.lookup c m of
      Just (Impl.IndexImpl cm) -> Ix.lookupRangeM k1 k2 cm
      _                        -> return []

-- | Range query in multiple contexts.
lookupRangeCxs :: Monad m => [Context] -> Text -> Text -> ContextMap v -> m [(Context, [(Text, v)])]
lookupRangeCxs cs k1 k2 (ContextMap m)
  = P.mapM search' cs
  where
  search' c = case M.lookup c m of
    Just (Impl.IndexImpl cm) -> do
      ix <- Ix.lookupRangeM k1 k2 cm
      return (c, ix)
    _ -> return (c, [])

-- | Search query in a context.
searchWithCx :: Monad m => TextSearchOp -> Context -> Text -> ContextMap v -> m [(Text, v)]
searchWithCx op c k (ContextMap m)
  = case M.lookup c m of
      Just (Impl.IndexImpl cm) -> Ix.searchM op k cm
      _                        -> return []

-- XXX: we actually do not have any parallelism here at the moment
--      because everything is evalutated lazy!
-- | Search query in multiple contexts.
searchWithCxs :: Monad m => TextSearchOp -> [Context] -> Text -> ContextMap v -> m [(Context, [(Text, v)])]
searchWithCxs op cs k (ContextMap m)
  = P.mapM search' cs
  where
  search' c = case M.lookup c m of
    Just (Impl.IndexImpl cm) -> do
      ix <- Ix.searchM op k cm
      return (c, ix)
    _ -> return (c, [])

-- | Search in contexts with key already normalized with respect to each context type.
searchWithCxsNormalized :: Monad m => TextSearchOp -> [(Context, Text)] -> ContextMap v -> m [(Context, [(Text, v)])]
searchWithCxsNormalized op cks (ContextMap m)
  = P.mapM search' cks
  where
  search' (c, k) = case M.lookup c m of
    Just (Impl.IndexImpl cm) -> do
      ix <- Ix.searchM op k cm
      return (c, ix)
    _ -> return (c, [])

-- ------------------------------------------------------------

-- | Contexts/keys of 'ContextMap'.
contexts' :: ContextMap v -> [Context]
contexts' (ContextMap m) = M.keys m

-- | Check if the context exists.
hasContext' :: Context -> ContextMap v -> Bool
hasContext' c (ContextMap m) = M.member c m

{--
 - this is unused right now
 - the algorithm is implemented within the index instance directly
 -
mapReduceAddWordsM :: Par.MonadParallel m => [(DocId, Words)] -> ContextMap Occurrences -> m (ContextMap Occurrences)
mapReduceAddWordsM vs (ContextMap m)
  = mapWithKeyMP (\cx impl -> mrInsert cx impl) m >>= return . ContextMap
  where
  mrInsert :: Par.MonadParallel m => Context -> IndexImpl Occurrences -> m (IndexImpl Occurrences)
  mrInsert cx ix@(Impl.IndexImpl impl)
    = case contentForCx cx vs of
        [] -> return ix
        cs -> do
          mapRes <- Par.mapM (\ws -> Ix.insertListM ws (Ix.empty `asTypeOf` impl)) (partitionListByLength 200 cs)
          reduce mapRes impl >>= return . Impl.mkIndex

  reduce mapRes impl = do
    x <- Par.mapM (\(i1,i2) -> Ix.unionWithM (Occ.merge) i1 i2) $ mkPairs mapRes
    case x of
      []     -> error "this should not be possible..?!?"
      [x]    -> Ix.unionWithM (Occ.merge) x impl
      xs     -> reduce xs impl


  mkPairs :: [a] -> [(a,a)]
  mkPairs []       = []
  mkPairs (a:[])   = [(a,a)]
  mkPairs (a:b:xs) = (a,b):mkPairs xs
--}
-- | Update elements
--update :: (Par.MonadParallel m, DocTable dt)
--       => DocId -> Dt.DValue dt -> Words
--       -> ContextIndex dt -> m (ContextIndex dt)
--update docId doc' w ix = do
--  ix' <- delete ix (IS.singleton docId)
--  insert doc' w ix'

-- ------------------------------------------------------------
