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
       , contextsM
       , hasContext
       , hasContextM

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
                                 -- is there a better approach to achieve this?
       , createDocTableFromPartition  -- only used in tests
       , unionDocTables               -- only used in tests
       , modifyWithDescription
       , delete
       , deleteDocsByURI
       , loadCxIx
       , member

       , lookupDocument
       , lookupDocuments

       , indexedWords

         -- * Types
       , ContextIndex (..)
       , ContextMap (..)
       , IndexRep
       , mkContextMap
       , mapToSchema
       )
where
{-
import           Debug.Trace (traceShow)
-- -}
import           Control.Arrow
import           Control.Monad
import qualified Control.Monad.Parallel       as Par
import           Data.Binary                  (Binary (..))
import           Data.Binary.Get
import           Data.ByteString.Lazy         (ByteString)
import           Data.Map                     (Map)
import qualified Data.Map                     as M
import           Data.Maybe
import           Data.Ord
import           Data.Set                     (Set)
import qualified Data.Set                     as S
import           Data.Text                    (Text)
import           Data.Traversable
import           Hunt.Common.BasicTypes       (Context, Description,
                                               TextSearchOp, URI, Word, Words)
import qualified Hunt.Common.DocDesc          as DocDesc
import           Hunt.Common.DocId            (DocId)
import           Hunt.Common.DocIdMap         (DocIdMap)
import           Hunt.Common.DocIdSet         (DocIdSet)
import qualified Hunt.Common.DocIdSet         as DS
import           Hunt.Common.Document         (Document (..))
import qualified Hunt.Common.Document         as Doc
import           Hunt.Common.Occurrences      (Occurrences)
import qualified Hunt.Common.Occurrences      as Occ
import           Hunt.DocTable                (DocTable)
import qualified Hunt.DocTable                as Dt
import           Hunt.DocTable.HashedDocTable (Documents)
import qualified Hunt.Index                   as Ix
import           Hunt.Index.IndexImpl         (IndexImpl)
import qualified Hunt.Index.IndexImpl         as Impl
import           Hunt.Index.Schema
import           Hunt.Scoring.Score           (Score, noScore)
import           Hunt.Scoring.SearchResult    (SearchResult)
import           Hunt.Utility
import           Prelude                      hiding (Word)
import qualified Prelude                      as P

-- ------------------------------------------------------------

-- | Context index introduces contexts and combines the major components of Hunt.

data ContextIndex = ContextIndex
  { ciIndex :: !ContextMap           -- ^ Indexes associated to contexts.
  , ciDocs  :: !(Documents Document) -- ^ Document table.
  }

empty :: ContextIndex
empty = ContextIndex emptyContextMap Dt.empty

-- | Contexts with associated heterogeneous index implementations.

type IndexRep = (ContextSchema, Impl.IndexImpl)

newtype ContextMap
  = ContextMap { cxMap :: Map Context IndexRep }
  deriving (Show)

-- | Empty context map.
emptyContextMap :: ContextMap
emptyContextMap = mkContextMap $ M.empty


-- | Strict smart constructor for the 'ContextMap'.

mkContextMap :: Map Context IndexRep -> ContextMap
mkContextMap x = ContextMap $! x

-- | Get 'Schema' from 'ContextMap'
mapToSchema :: ContextMap -> Schema
mapToSchema (ContextMap m) = M.map fst m

-- ------------------------------------------------------------
-- Binary / Serialization
-- ------------------------------------------------------------

getContextMap :: [IndexImpl] -> Get ContextMap
getContextMap ts
  = do
    impls <- Impl.gets' ts
    s     <- get
    return . mkContextMap . M.fromDistinctAscList $ map (mergeGets s) impls
    where
      mergeGets s (c, i) = (c, (getS c s, i))
      getS c s = fromMaybe (error "deserializating failed: context schema is missing")
               $ lookup c s

instance Binary ContextMap where
  put = put . cxMap
  get = get >>= return . mkContextMap


-- | Deserialize a 'ContextIndex' with the list of available index implementations and a
--   map of available 'ContextSchema'.
--
--   /Note/: The serialized index implementations have to  be in the list of available types,
--           otherwise this will fail. The serialized schemas have to be in the list of
--           available 'ContextSchema', otherwise this will fail as well.
loadCxIx :: Monad m
         => [IndexImpl]
         -> (Text -> m ContextType)
         -> (Text -> m CNormalizer)
         -> ByteString
         -> m ContextIndex
loadCxIx ixImpls getCxType getNormalizers bytes =
  reloadSchema getCxType getNormalizers (decodeCxIx ixImpls bytes)

decodeCxIx :: [IndexImpl] -> ByteString -> ContextIndex
decodeCxIx ts = runGet (get' ts)

reloadSchema :: Monad m
             => (Text -> m ContextType)
             -> (Text -> m CNormalizer)
             -> ContextIndex
             -> m ContextIndex
reloadSchema getCxType getNormalizers (ContextIndex cx docs) = do
  cx' <- forM (cxMap cx) $ \(s, ix) -> do
    cxt <- getCxType . ctName . cxType $ s
    ns  <- mapM (getNormalizers . cnName) (cxNormalizer s)
    return ( s { cxType       = cxt
               , cxNormalizer = ns
               }, ix)

  return ContextIndex { ciIndex = mkContextMap cx'
                      , ciDocs  = docs
                      }

get' :: [IndexImpl] -> Get (ContextIndex)
get' ts = ContextIndex <$> (getContextMap ts) <*> get

instance Binary (ContextIndex) where
  get = error "existential types cannot be deserialized this way. Use special get' functions"
  put (ContextIndex (ContextMap a) b)
    = put (M.map snd a) >>  -- convert to 'IndexImpl' and serialize
      put (M.map fst a) >>  -- convert to 'Schema' and serialize
      put b                 -- put 'DocTable'

-- ------------------------------------------------------------

{-
-- | Insert a Document and Words.
--
--   /Note/: For multiple inserts, use the more efficient 'insertList'.
insert :: (Par.MonadParallel m, Applicative m, DocTable dt)
       => Dt.DValue dt -> Words -> ContextIndex -> m (ContextIndex)
insert doc wrds ix = insertList [(doc,wrds)] ix
-}

--   This is more efficient than using fold and with 'insert'.
-- | Insert multiple documents and words.

insertList :: (Par.MonadParallel m, Applicative m) =>
              [(Document, Words)] ->
              ContextIndex -> m (ContextIndex)

insertList docAndWords (ContextIndex ix docTable)
    = do -- insert to doctable and generate docId
         tablesAndWords <- Par.mapM createDocTableFromPartition
                         $ partitionListByLength 20 docAndWords
         -- union doctables and docid-words pairs
         (newDt, docIdsAndWords) <- unionDocTables tablesAndWords docTable
         -- insert words to index
         newIx <- batchAddWordsM docIdsAndWords ix
         return $! ContextIndex newIx newDt

-- takes list of documents with wordlist. creates new 'DocTable' and
-- inserts each document of the list into it.
createDocTableFromPartition :: (Par.MonadParallel m)
                            => [(Document, Words)]
                            -> m (Documents Document, [(DocId, Words)])
createDocTableFromPartition ds
    = foldM toDocTable (Dt.empty, []) ds
    where
      toDocTable (dt, resIdsAndWords) (doc, ws)
        = do (dId, dt') <- Dt.insert doc dt
             return (dt', (dId, ws):resIdsAndWords)

-- takes list of doctables with lists of docid-words pairs attached
-- unions the doctables to one big doctable and concats the docid-words
-- pairs to one list
unionDocTables :: (Par.MonadParallel m)
               => [(Documents Document, [(DocId, Words)])]
               -> Documents Document
               -> m (Documents Document, [(DocId, Words)])
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
       -> Words -> DocId -> ContextIndex -> m (ContextIndex)
modify f wrds dId (ContextIndex ii dt s) = do
  newDocTable <- Dt.adjust f dId dt
  newIndex    <- addWordsM wrds dId ii
  return $ ContextIndex newIndex newDocTable s
-- -}

lookupDocument :: Monad m => ContextIndex -> DocId -> m (Maybe Document)
lookupDocument (ContextIndex _ dt) did = Dt.lookup did dt

lookupDocuments :: Monad m => ContextIndex -> DocIdSet -> m (DocIdMap Document)
lookupDocuments ixx dids = do
  dt' <- Dt.restrict dids (ciDocs ixx)
  Dt.toMap dt'

-- | Delete a set of documents by 'URI'.
deleteDocsByURI :: (Par.MonadParallel m, Applicative m)
                => Set URI -> ContextIndex -> m (ContextIndex)
deleteDocsByURI us ixx@(ContextIndex _ix dt) = do
  docIds <- liftM (DS.fromList . catMaybes) . mapM (flip Dt.lookupByURI dt) . S.toList $ us
  delete docIds ixx


-- | Delete a set of documents by 'DocId'.
delete :: (Par.MonadParallel m, Applicative m)
       => DocIdSet -> ContextIndex -> m (ContextIndex)
delete dIds cix@(ContextIndex ix dt)
    | DS.null dIds
        = return cix
    | otherwise
        = do newIx <- delete' dIds ix
             newDt <- Dt.difference dIds dt
             return $ ContextIndex newIx newDt


-- | Is the document part of the index?
member :: (Monad m, Applicative m)
       => URI -> ContextIndex -> m Bool
member u (ContextIndex _ii dt) = do
  mem <- Dt.lookupByURI u dt
  return $ isJust mem

-- ------------------------------------------------------------

-- | Modify the description of a document and add words
--   (occurrences for that document) to the index.

modifyWithDescription :: (Par.MonadParallel m, Applicative m) =>
                         Score -> Description -> Words -> DocId -> ContextIndex ->
                         m (ContextIndex)
modifyWithDescription weight descr wrds dId (ContextIndex ii dt)
    = do newDocTable <- Dt.adjust mergeDescr dId dt
         newIndex    <- batchAddWordsM [(dId,wrds)] ii
         return $ ContextIndex newIndex newDocTable
    where
      -- M.union is left-biased
      -- flip to use new values for existing keys
      -- no flip to keep old values
      --
      -- Null values in new descr will remove associated attributes
      mergeDescr
          = return . Doc.update (updateWeight . updateDescr)
          where
            updateWeight d
                | weight == noScore = d
                | otherwise         = d {wght = weight}

            updateDescr d           = -- trc "updateDescr res=" $
                                      d {desc = DocDesc.deleteNull $
                                                flip DocDesc.union d' descr'
                                        }
                                      where
                                        d'     = -- trc "updateDescr old=" $
                                                 desc d
                                        descr' = -- trc "updateDescr new=" $
                                                 descr

-- trc :: Show a => String -> a -> a
-- trc msg x = traceShow (msg, x) x

-- ------------------------------------------------------------
-- Helper
-- ------------------------------------------------------------

-- | Adds words associated to a document to the index.
--

-- | Add words for a document to the 'Index'.
--
--   /Note/: Adds words to /existing/ 'Context's.

batchAddWordsM :: (Functor m, Par.MonadParallel m) =>
                  [(DocId, Words)] -> ContextMap -> m ContextMap
batchAddWordsM [] ix
    = return ix

batchAddWordsM vs (ContextMap m)
    = mkContextMap <$>
      mapWithKeyMP ( \cx (s, impl) -> do i <- foldinsertList cx impl
                                         return (s, i)
                   ) m
    where
      foldinsertList :: (Functor m, Monad m) =>
                        Context -> IndexImpl -> m IndexImpl
      foldinsertList cx (Impl.IndexImpl impl)
          = Impl.mkIndex <$>
            Ix.insertListM (contentForCx cx vs) impl

-- | Computes the words and occurrences out of a list for one context

contentForCx :: Context -> [(DocId, Words)] -> [(Word, Occurrences)]
contentForCx cx vs
    = concatMap (invert . second (getWlForCx cx)) $ vs
          where
            invert (did, wl)
                = map (second (Occ.singleton' did)) $ M.toList wl
            getWlForCx cx' ws'
                = fromMaybe M.empty (M.lookup cx' ws')

----------------------------------------------------------------------------
-- addWords/batchAddWords functions
----------------------------------------------------------------------------

mapWithKeyMP :: (Par.MonadParallel m, Ord k) => (k -> a -> m b) -> M.Map k a -> m (M.Map k b)
mapWithKeyMP f m =
  (Par.mapM (\(k, a) -> do
                  b <- f k a
                  return (k, b)
                ) $ M.toList m) >>=
    return . M.fromList

----------------------------------------------------------------------------

-- | Inserts a new context.
--
insertContext :: Context -> Impl.IndexImpl -> ContextSchema
              -> ContextIndex -> ContextIndex
insertContext c ix schema (ContextIndex m dt)
    = ContextIndex  m' dt
    where
    m' = insertContext' c schema ix m

--   /Note/: Does nothing if the context already exists.
insertContext' :: Context -> ContextSchema -> Impl.IndexImpl -> ContextMap -> ContextMap
insertContext' c s ix (ContextMap m) = mkContextMap $ M.insertWith (const id) c (s, ix) m

-- | Removes context (including the index and the schema).
deleteContext :: Context -> ContextIndex -> ContextIndex
deleteContext c (ContextIndex ix dt) = ContextIndex (deleteContext' c ix) dt

-- | Removes context (includes the index, but not the schema).
deleteContext' :: Context -> ContextMap -> ContextMap
deleteContext' cx (ContextMap m) = mkContextMap $ M.delete cx m

delete' :: Par.MonadParallel m => DocIdSet -> ContextMap -> m ContextMap
delete' dIds (ContextMap m)
  = mapWithKeyMP (\_ impl -> adjust' impl) m >>= return . mkContextMap
--  = TV.mapM adjust' m >>= return . mkContextMap
  where
  adjust' (s, Impl.IndexImpl ix) = Ix.deleteDocsM dIds ix >>= \i -> return (s, Impl.mkIndex i)

{- not yet used

-- | Search query in all context.
search :: Monad m => TextSearchOp -> Text -> ContextMap -> m [(Context, [(Text, v)])]
search op k (ContextMap m)
  = liftM M.toList $ TV.mapM search' m
  where
  search' (Impl.IndexImpl ix) = Ix.searchM op k ix
-- -}

-- | Range query in a context between first and second key.
lookupRangeCx :: Monad m => Context -> Text -> Text -> ContextMap -> m [(Text, SearchResult)]
lookupRangeCx c k1 k2 cm
    = lookupIndex c cm $ Ix.lookupRangeM k1 k2

-- | Dump a context
lookupAllWithCx :: Monad m => Context -> ContextMap -> m [(Text, SearchResult)]
lookupAllWithCx cx cm
    = lookupIndex cx cm $ Ix.toListM

-- | Search query in a context.
searchWithCx :: Monad m => TextSearchOp -> Context -> Text -> ContextMap -> m [(Text, SearchResult)]
searchWithCx op cx w cm
    = lookupIndex cx cm $ Ix.searchM op w


-- | Search over a list of contexts and words
searchWithCxsNormalized :: (Functor m, Monad m) =>
                           TextSearchOp -> [(Context, Text)] -> ContextMap ->
                           m [(Context, [(Text, SearchResult)])]
searchWithCxsNormalized op cxws cm
    = P.mapM (uncurry search') cxws
    where
      search' cx w
          = (\ x -> (cx, x))
            <$> (lookupIndex cx cm $ Ix.searchM op w)

searchWithCxSc :: Monad m
               => TextSearchOp
               -> Context
               -> Text
               -> ContextIndex
               -> m [(Text, (Score, SearchResult))]
searchWithCxSc op cx w ixx
    = lookupIndex cx (ciIndex ixx) $ Ix.searchMSc op w

lookupRangeCxSc :: Monad m
                => Context
                -> Text
                -> Text
                -> ContextIndex
                -> m [(Text, (Score, SearchResult))]
lookupRangeCxSc c k1 k2 ixx
    = lookupIndex c (ciIndex ixx) $ Ix.lookupRangeMSc k1 k2

-- ------------------------------------------------------------

-- | lookup an index by a context and then search this index for a word
-- result is always a list of values.
--
-- This pattern is used in all search variants

lookupIndex :: Monad m =>
               Context -> ContextMap ->
               (forall i . Impl.IndexImplCon i => i -> m [r]) ->
               m [r]
lookupIndex cx (ContextMap m) search
    = case M.lookup cx m of
        Just (_, Impl.IndexImpl cm)
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

-- | All contexts of the index.
contextsM :: (Monad m)
         => ContextIndex -> m [Context]
contextsM = return . contexts

-- | Contexts/keys of 'ContextMap'.
contexts :: ContextIndex -> [Context]
contexts = M.keys . cxMap . ciIndex

-- | Check if the context exists.
hasContext :: Context -> ContextMap -> Bool
hasContext c (ContextMap m) = M.member c m

-- | Does the context exist?
hasContextM :: (Monad m)
           => Context -> ContextIndex -> m Bool
hasContextM c (ContextIndex ix _) = return $ hasContext c ix

-- | Return the indexed words along with their occurrences in their contexts.
indexedWords :: Monad m => ContextIndex -> m [(Text, Map Context SearchResult)]
indexedWords ixx = do
  wx <- for (M.toList (cxMap (ciIndex ixx))) $ \(context, (schema, Impl.IndexImpl ix)) -> do
    return [ (word, M.singleton context result)
           | (word, result) <- Ix.toList ix
           ]
  return $ foldr (merge (comparing fst) mappend) [] wx
  where
    merge :: (a -> a -> Ordering) -> (a -> a -> a) -> [a] -> [a] -> [a]
    merge _ _ xs [] = xs
    merge _ _ [] xs = xs
    merge cmp f a@(h:first) b@(c:second) =
      case cmp h c of
        LT -> h:merge cmp f first b
        EQ -> f h c: merge cmp f first b
        GT -> c:merge cmp f a second
