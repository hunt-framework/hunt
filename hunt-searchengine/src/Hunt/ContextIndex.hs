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
  , foreachContext
  , contexts
  , contexts'
  , hasContext'
  , hasContext

    -- * Queries
  , lookupRangeCx
  , lookupAllWithCx
  , searchWithCx
  , searchWithCxsNormalized
  , searchWithCxSc
  , lookupRangeCxSc

    -- * Insert\/Delete Documents
  , insertList
                                 -- XXX: these functions should be internal
                                 -- we export them to be able to test them
                                 -- is there a bedder approach to achieve this?
  , createDocTableFromPartition  -- only used in tests
  , unionDocTables               -- only used in tests
  , insertWithCx                 -- only used in tests
  , modifyWithDescription
  , addWordsM                    -- only used in tests
  , delete
  , deleteDocsByURI
  , decodeCxIx
  , member

    -- * Types
  , ContextIndex (..)
  , ContextMap (..)
  )
where
{-
import           Debug.Trace             (traceShow)
-- -}
import           Prelude
import qualified Prelude                 as P

import           Control.Applicative     (Applicative, (<$>), (<*>))
import           Control.Arrow
import           Control.Monad
import qualified Control.Monad.Parallel  as Par
{-
import           Control.Parallel.Strategies
-- -}
import           Data.Binary             (Binary (..))
import           Data.Binary.Get
import           Data.ByteString.Lazy    (ByteString)
import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Maybe
import           Data.Set                (Set)
import qualified Data.Set                as S
import           Data.Text               (Text)
{-
import qualified Data.Traversable        as TV
-- -}

import           Hunt.Common
import qualified Hunt.Common.DocDesc     as DD
import qualified Hunt.Common.DocIdSet    as DS
import qualified Hunt.Common.Document    as Doc
import qualified Hunt.Common.Occurrences as Occ
import           Hunt.DocTable           (DocTable)
import qualified Hunt.DocTable           as Dt
import qualified Hunt.Index              as Ix
import           Hunt.Index.IndexImpl    (IndexImpl)
import qualified Hunt.Index.IndexImpl    as Impl
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
getContextMap ts = mkContextMap <$>
                   liftM M.fromDistinctAscList (Impl.gets' ts)

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

insertList :: (Par.MonadParallel m, Applicative m, DocTable dt) =>
              [(Dt.DValue dt, Words)] ->
              ContextIndex dt -> m (ContextIndex dt)

insertList docAndWords (ContextIndex ix docTable s)
    = do -- insert to doctable and generate docId
         tablesAndWords <- Par.mapM createDocTableFromPartition
                         $ partitionListByLength 20 docAndWords
         -- union doctables and docid-words pairs
         (newDt, docIdsAndWords) <- unionDocTables tablesAndWords docTable
         -- insert words to index
         newIx <- batchAddWordsM docIdsAndWords ix
         return $! ContextIndex newIx newDt s

-- takes list of documents with wordlist. creates new 'DocTable' and
-- inserts each document of the list into it.
createDocTableFromPartition :: (Par.MonadParallel m, DocTable dt) =>
                               [(Dt.DValue dt, Words)] -> m (dt, [(DocId, Words)])
createDocTableFromPartition ds
    = foldM toDocTable (Dt.empty, []) ds
    where
      toDocTable (dt, resIdsAndWords) (doc, ws)
        = do (dId, dt') <- Dt.insert doc dt
             return (dt', (dId, ws):resIdsAndWords)

-- takes list of doctables with lists of docid-words pairs attached
-- unions the doctables to one big doctable and concats the docid-words
-- pairs to one list
unionDocTables :: (DocTable dt, Par.MonadParallel m) =>
                  [(dt, [(DocId, Words)])] -> dt -> m (dt, [(DocId, Words)])
unionDocTables tablesAndWords oldDt
    = do step <- Par.mapM unionDtsAndWords $ mkPairs tablesAndWords
         case step of
                 []      -> return (Dt.empty, [])
                 [(d,w)] -> do n <- Dt.union oldDt d
                               return (n, w)
                 xs      -> unionDocTables xs oldDt
    where
      unionDtsAndWords ((dt1, ws1), (dt2, ws2))
        = do dt <- Dt.union dt1 dt2
             return (dt, ws1 ++ ws2)

      mkPairs []       = []
      mkPairs (a:[])   = [(a,(Dt.empty,[]))]
      mkPairs (a:b:xs) = (a,b):mkPairs xs

{-
-- XXX: this should not work well atm
-- | Modify documents and index data.

modify :: (Par.MonadParallel m, Applicative m, DocTable dt)
       => (Dt.DValue dt -> m (Dt.DValue dt))
       -> Words -> DocId -> ContextIndex dt -> m (ContextIndex dt)
modify f wrds dId (ContextIndex ii dt s) = do
  newDocTable <- Dt.adjust f dId dt
  newIndex    <- addWordsM wrds dId ii
  return $ ContextIndex newIndex newDocTable s
-- -}

-- | Delete a set of documents by 'URI'.
deleteDocsByURI :: (Par.MonadParallel m, Applicative m, DocTable dt)
                => Set URI -> ContextIndex dt -> m (ContextIndex dt)
deleteDocsByURI us ixx@(ContextIndex _ix dt _) = do
  docIds <- liftM (DS.fromList . catMaybes) . mapM (flip Dt.lookupByURI dt) . S.toList $ us
  delete docIds ixx


-- | Delete a set of documents by 'DocId'.
delete :: (Par.MonadParallel m, Applicative m, DocTable dt)
       => DocIdSet -> ContextIndex dt -> m (ContextIndex dt)
delete dIds cix@(ContextIndex ix dt s)
    | DS.null dIds
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

modifyWithDescription :: (Par.MonadParallel m, Applicative m, DocTable dt) =>
                         Score -> Description -> Words -> DocId -> ContextIndex dt ->
                         m (ContextIndex dt)
modifyWithDescription weight descr wrds dId (ContextIndex ii dt s)
    = do newDocTable <- Dt.adjust mergeDescr dId dt
         newIndex    <- addWordsM wrds dId ii
         return $ ContextIndex newIndex newDocTable s
    where
      -- M.union is left-biased
      -- flip to use new values for existing keys
      -- no flip to keep old values
      mergeDescr
          = return . Doc.update (updateWeight . updateDescr)
          where
            updateWeight d
                | weight == noScore = d
                | otherwise         = d {wght = weight}
            updateDescr d           = d {desc = flip DD.union (desc d) descr}

-- ------------------------------------------------------------
-- Helper
-- ------------------------------------------------------------

-- | Adds words associated to a document to the index.
--
--   'insertList' and 'modifyWithDescription' are more convenient in most cases.
addWordsM :: (Functor m, Par.MonadParallel m) =>
             Words -> DocId -> ContextMap Occurrences -> m (ContextMap Occurrences)
addWordsM wrds dId
    = batchAddWordsM [(dId, wrds)]

-- | Add words for a document to the 'Index'.
--
--   /Note/: Adds words to /existing/ 'Context's.

batchAddWordsM :: (Functor m, Par.MonadParallel m) =>
                  [(DocId, Words)] -> ContextMap Occurrences -> m (ContextMap Occurrences)
batchAddWordsM vs (ContextMap m)
  = mkContextMap <$> mapWithKeyMP (\cx impl -> foldinsertList cx impl) m
  where
    foldinsertList :: (Functor m, Monad m) =>
                      Context -> IndexImpl Occurrences -> m (IndexImpl Occurrences)
    foldinsertList cx (Impl.IndexImpl impl)
        = Impl.mkIndex <$> Ix.insertListM Occ.merge (contentForCx cx vs) impl

-- | Computes the words and occurrences out of a list for one context

contentForCx :: Context -> [(DocId, Words)] -> [(Word, Occurrences)]
contentForCx cx vs
    = res
    where
      res = concatMap (invert . second (getWlForCx cx)) $ vs
          where
            invert (did, wl)
                = map (second (Occ.singleton' did)) $ M.toList wl
            getWlForCx cx' ws'
                = fromMaybe M.empty (M.lookup cx' ws')

-- | Add words for a document to the 'Index'.
--
--   /Note/: Adds words to /existing/ 'Context's.

{-- OLD
batchAddWords :: [(DocId, Words)] -> ContextMap Occurrences -> ContextMap Occurrences
batchAddWords vs (ContextMap m)
  = mkContextMap $ M.fromList $ {- XXX parMap rpar -} map (\(cx,impl) -> (cx,foldinsertList cx impl)) (M.toList m)
  where
  foldinsertList :: Context -> IndexImpl Occurrences-> IndexImpl Occurrences
  foldinsertList cx (Impl.IndexImpl impl)
    = Impl.mkIndex $ Ix.insertList Occ.merge (contentForCx cx vs) impl
--}

----------------------------------------------------------------------------
-- addWords/batchAddWords functions
----------------------------------------------------------------------------

{- a reference impl for the parallel mapM

mapWithKeyP :: (Monad m, Ord k) => (k -> a -> m b) -> M.Map k a -> m (M.Map k b)
mapWithKeyP f m =
  (P.mapM (\(k, a) -> do
                  b <- f k a
                  return (k, b)
                ) $ M.toList m) >>=
    return . M.fromList
-- -}

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
insertWithCx :: Monad m =>
                (v -> v -> v) ->
                Context -> Text -> v -> ContextMap v -> m (ContextMap v)
insertWithCx op c w v (ContextMap m)
  = case M.lookup c m of
      Just (Impl.IndexImpl ix) -> do
        ix' <- liftM Impl.mkIndex $ Ix.insertListM op [(w,v)] ix
        return $ ContextMap $ M.insert c ix' m
      _      -> error "context does not exist"

delete' :: Par.MonadParallel m => DocIdSet -> ContextMap v -> m (ContextMap v)
delete' dIds (ContextMap m)
  = mapWithKeyMP (\_ impl -> adjust' impl) m >>= return . mkContextMap
--  = TV.mapM adjust' m >>= return . mkContextMap
  where
  adjust' (Impl.IndexImpl ix) = liftM Impl.mkIndex $ Ix.deleteDocsM dIds ix

{- not yet used

-- | Search query in all context.
search :: Monad m => TextSearchOp -> Text -> ContextMap v -> m [(Context, [(Text, v)])]
search op k (ContextMap m)
  = liftM M.toList $ TV.mapM search' m
  where
  search' (Impl.IndexImpl ix) = Ix.searchM op k ix
-- -}

-- | Range query in a context between first and second key.
lookupRangeCx :: Monad m => Context -> Text -> Text -> ContextMap v -> m [(Text, v)]
lookupRangeCx c k1 k2 cm
    = lookupIndex c cm $ Ix.lookupRangeM k1 k2

-- | Dump a context
lookupAllWithCx :: Monad m => Context -> ContextMap v -> m [(Text, v)]
lookupAllWithCx cx cm
    = lookupIndex cx cm $ Ix.toListM

-- | Search query in a context.
searchWithCx :: Monad m => TextSearchOp -> Context -> Text -> ContextMap v -> m [(Text, v)]
searchWithCx op cx w cm
    = lookupIndex cx cm $ Ix.searchM op w


-- | Search over a list of contexts and words
searchWithCxsNormalized :: (Functor m, Monad m) =>
                           TextSearchOp -> [(Context, Text)] -> ContextMap v ->
                           m [(Context, [(Text, v)])]
searchWithCxsNormalized op cxws cm
    = P.mapM (uncurry search') cxws
    where
      search' cx w
          = (\ x -> (cx, x))
            <$> (lookupIndex cx cm $ Ix.searchM op w)

-- | Search query with scored results
searchWithCxSc :: Monad m =>
                  TextSearchOp -> Context -> Text -> ContextMap v -> m [(Text, (Score, v))]
searchWithCxSc op cx w cm
    = lookupIndex cx cm $ Ix.searchMSc op w

-- | Range query in a context between first and second key.
lookupRangeCxSc :: Monad m => Context -> Text -> Text -> ContextMap v -> m [(Text, (Score, v))]
lookupRangeCxSc c k1 k2 cm
    = lookupIndex c cm $ Ix.lookupRangeMSc k1 k2

-- ------------------------------------------------------------

-- | lookup an index by a context and then search this index for a word
-- result is always a list of values.
--
-- This pattern is used in all search variants

lookupIndex :: (Monad m) =>
               Context -> ContextMap v ->
               (forall i . Impl.IndexImplCon i v => i v -> m [r]) ->
               m [r]

lookupIndex cx (ContextMap m) search
    = case M.lookup cx m of
        Just (Impl.IndexImpl cm)
            -> search cm
        Nothing
            -> return []

foreachContext :: (Functor m, Monad m) =>
                  [Context] ->
                  (Context -> m res) ->
                  m [(Context, res)]
foreachContext cxs action
    = P.mapM action' cxs
      where
        action' cx
            = (\ r -> (cx, r)) <$> action cx

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
