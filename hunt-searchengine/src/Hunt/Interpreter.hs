{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

-- ----------------------------------------------------------------------------
{- |
  The interpreter to evaluate 'Command's.
-}
-- ----------------------------------------------------------------------------

module Hunt.Interpreter
  ( -- * Initialization
    initHunt
    -- * Running Commands
  , runCmd
  , execCmd
  , runHunt
    -- * Types
  , Hunt
  , HuntT (..)
  , HuntEnv (..)
  , DefHuntEnv
  )
where

import           Control.Arrow                 (second)
import           Control.Concurrent.STM
import           Control.Concurrent.XMVar
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Aeson                    (ToJSON (..), object, (.=))
import           Data.Binary                   (Binary, encodeFile)
import           Data.Default
import           Data.IORef
import qualified Data.List                     as L
import qualified Data.Map                      as M
import           Data.Monoid
import           Data.Set                      (Set)
import qualified Data.Set                      as S
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Traversable              as Trav

import           Hunt.Common.ApiDocument       as ApiDoc
import           Hunt.Common.BasicTypes        (Context, URI)
import qualified Hunt.Common.DocDesc           as DocDesc
import qualified Hunt.Common.DocIdMap          as DocIdMap
import qualified Hunt.Common.DocIdSet          as DocIdSet
import           Hunt.Common.Document          (Document (..), unwrap)
import           Hunt.ContextIndex             (ContextIndex)
import qualified Hunt.ContextIndex             as CIx
import           Hunt.ContextIndex.Flush       (FlushPolicy (..))
import qualified Hunt.ContextIndex.Flush       as Flush
import           Hunt.ContextIndex.Merge       (MergePolicy (..))
import qualified Hunt.ContextIndex.Merge       as Merge
import           Hunt.DocTable                 (DocTable)
import           Hunt.DocTable.HashedDocTable
import qualified Hunt.Index                    as Ix
import           Hunt.Index.IndexImpl          (IndexImpl (..), mkIndex)
import           Hunt.Index.Schema
import           Hunt.Index.Schema.Analyze
import           Hunt.Interpreter.BasicCommand
import           Hunt.Interpreter.Command      (Command)
import           Hunt.Interpreter.Command      hiding (Command (..))
import           Hunt.Interpreter.Worker       (Worker)
import qualified Hunt.Interpreter.Worker       as Worker
import           Hunt.Query.Intermediate       (RankedDoc (..), ScoredWords,
                                                toDocsResult,
                                                toDocumentResultPage,
                                                toWordsResult)
import           Hunt.Query.Language.Grammar
import           Hunt.Query.Processor          (ProcessConfig (..),
                                                initProcessor, mkQueryIndex,
                                                processQueryScoredDocs,
                                                processQueryScoredWords,
                                                processQueryUnScoredDocs)
import           Hunt.Scoring.SearchResult     (ScoredDocs, UnScoredDocs,
                                                scoredDocsToDocIdSet,
                                                searchResultToOccurrences,
                                                unScoredDocsToDocIdSet)
import qualified Hunt.Segment                  as Segment
import           Hunt.Utility                  (showText)
import           Hunt.Utility.Log

import           System.IO.Error               (isAlreadyInUseError,
                                                isFullError, isPermissionError,
                                                tryIOError)
import qualified System.Log.Logger             as Log

import           GHC.Stats                     (getGCStats, getGCStatsEnabled)
import           GHC.Stats.Json                ()

-- ------------------------------------------------------------
--
-- the semantic domains (datatypes for interpretation)
--
-- HuntEnv, Index, ...

-- ------------------------------------------------------------
--
-- the indexer used in the interpreter
-- this should be a generic interpreter in the end
-- but right now its okay to have the indexer
-- replaceable by a type declaration

-- ------------------------------------------------------------
-- Logging

-- TODO: manage exports

-- | Name of the module for logging purposes.
modName :: String
modName = "Hunt.Interpreter"

-- | Log a message at 'DEBUG' priority.
debugM :: MonadIO m => String -> m ()
debugM = liftIO . Log.debugM modName
{-
-- | Log a message at 'WARNING' priority.
warningM :: MonadIO m => String -> m ()
warningM = liftIO . Log.warningM modName
-}
-- | Log a message at 'ERROR' priority.
errorM :: MonadIO m => String -> m ()
errorM = liftIO . Log.errorM modName

{-
-- | Log formated values that get inserted into a context
debugContext :: Context -> Words -> IO ()
debugContext c ws = debugM $ concat ["insert in ", T.unpack c, show . M.toList $ fromMaybe M.empty $ M.lookup c ws]
-- -}

-- ------------------------------------------------------------

-- | The Hunt state and environment.
--   Initialize with default values with 'initHunt'.
data HuntEnv dt = HuntEnv
  { -- | The context index (indexes, document table and schema).
    --   Stored in an 'XMVar' so that read access is always possible.
    huntIndex       :: DocTable dt => XMVar (ContextIndex dt)
    -- | Describes how and where to flush the index.
  , huntFlushPolicy :: FlushPolicy
    -- | Merge policy for index construction
  , huntMergePolicy :: MergePolicy
    -- | Available context types.
  , huntTypes       :: ContextTypes
    -- | Available custom tokenizers
  , huntTokenizers  :: [CTokenizer]
    -- | Available normalizers.
  , huntNormalizers :: [CNormalizer]
    -- | Query processor configuration.
  , huntQueryCfg    :: ProcessConfig
    -- | Manges merges and commits.
  , huntIndexWorker :: IndexWorker
  }

-- | Default Hunt environment type.
type DefHuntEnv = HuntEnv (Documents Document)

-- | Initialize the Hunt environment with default values.
initHunt :: DocTable dt => IO (HuntEnv dt)
initHunt = initHuntEnv CIx.empty flushPolicy mergePolicy contextTypes [] normalizers def

-- | Default context types.
contextTypes :: ContextTypes
contextTypes = [ctText, ctInt, ctDate, ctPosition, ctTextSimple, ctPositionRTree]

-- | Default normalizers.
normalizers :: [CNormalizer]
normalizers = [cnUpperCase, cnLowerCase, cnZeroFill]

-- | Default merge policy
mergePolicy :: MergePolicy
mergePolicy
  = MergePolicy { mpMergeFactor       = 6
                , mpMinMerge          = 400
                , mpMaxParallelMerges = 2
                }

-- | Default flush policy
flushPolicy :: FlushPolicy
flushPolicy =
  FlushPolicy { fpFlushDirectory = "index"
              }

-- | Initialize the Hunt environment.
initHuntEnv :: DocTable dt
           => ContextIndex dt
           -> FlushPolicy
           -> MergePolicy
           -> ContextTypes
           -> [CTokenizer]
           -> [CNormalizer]
           -> ProcessConfig
           -> IO (HuntEnv dt)
initHuntEnv ixx fp mp opt tk ns qc = do
  ixref  <- newXMVar ixx
  flsr <- newIndexFlusher ixref fp
  mrgr <- newIndexMerger ixref mp
  return $ HuntEnv ixref fp mp opt tk ns qc (flsr <> mrgr)

-- ------------------------------------------------------------
-- Command evaluation monad
-- ------------------------------------------------------------

-- | The Hunt transformer monad. Allows a custom monad to be embedded to combine with other DSLs.

newtype HuntT dt m a
    = HuntT { runHuntT :: ReaderT (HuntEnv dt) (ExceptT CmdError m) a }
      deriving
      (Applicative, Monad, MonadIO, Functor, MonadReader (HuntEnv dt), MonadError CmdError)

instance MonadTrans (HuntT dt) where
  lift = HuntT . lift . lift

-- | The Hunt monad on 'IO'.
type Hunt dt = HuntT dt IO

-- ------------------------------------------------------------

-- | Run the Hunt monad with the supplied environment/state.

runHunt :: DocTable dt => HuntT dt m a -> HuntEnv dt -> m (Either CmdError a)
runHunt env = runExceptT . runReaderT (runHuntT env)

-- | Run the command the supplied environment/state.
runCmd :: (DocTable dt, Binary dt) => HuntEnv dt -> Command -> IO (Either CmdError CmdResult)
runCmd env cmd
  = runExceptT . runReaderT (runHuntT . execCmd $ cmd) $ env

-- | Get the context index.
askIx :: DocTable dt => Hunt dt (ContextIndex dt)
askIx = do
  ref <- asks huntIndex
  liftIO $ readXMVar ref

-- FIXME: io exception-safe?
-- | Modify the context index.
modIx :: DocTable dt
      => (ContextIndex dt -> Hunt dt (ContextIndex dt, a)) -> Hunt dt a
modIx f = do
  ref <- asks huntIndex
  ix <- liftIO $ takeXMVarWrite ref
  (!i',a) <- f ix `catchError` putBack ref ix
  liftIO $ putXMVarWrite ref i'
  return a
  where
  putBack ref i e = do
    liftIO $ putXMVarWrite ref i
    throwError e

-- | Modify the context index.
--   Locks the index for reads and writes to prevent excessive memory usage.
--
--   /Note/: This does not fix the memory issues on load entirely because the old index might
--   still be referenced by a concurrent read operation.
_modIxLocked :: DocTable dt
            => (ContextIndex dt -> Hunt dt (ContextIndex dt, a)) -> Hunt dt a
_modIxLocked f = do
  ref <- asks huntIndex
  ix <- liftIO $ takeXMVarLock ref
  (i',a) <- f ix `catchError` putBack ref ix
  liftIO $ putXMVarLock ref i'
  return a
  where
  putBack ref i e = do
    liftIO $ putXMVarLock ref i
    throwError e

-- | Do something with the context index.
withIx :: DocTable dt => (ContextIndex dt -> Hunt dt a) -> Hunt dt a
withIx f
  = askIx >>= f

-- | Get the type of a context.
askType :: DocTable dt => Text -> Hunt dt ContextType
askType cn = do
  ts <- asks huntTypes
  case L.find (\t -> cn == ctName t) ts of
    Just t -> return t
    _      -> throwResError 410 ("used unavailable context type: " `T.append` cn)

-- | Get the normalizer of a context.
askNormalizer :: DocTable dt => Text -> Hunt dt CNormalizer
askNormalizer cn = do
  ts <- asks huntNormalizers
  case L.find (\t -> cn == cnName t) ts of
    Just t -> return t
    _      -> throwResError 410 ("used unavailable normalizer: " `T.append` cn)

askTokenizer :: DocTable dt => CTokenizer -> Hunt dt CTokenizer
askTokenizer (CTokenizer tt@(TokenizeCustom name) _ )
  = do tks <- asks huntTokenizers
       case L.find (\t -> tt == ctkType t) tks of
         Just t -> return t
         _      -> throwResError 410 ("used unavailable tokenizer" `T.append` name)
askTokenizer (CTokenizer tt _ )
  = return (mkDefaultTokenizer tt)

-- | Get the index.
askIndex :: DocTable dt => Text -> Hunt dt IndexImpl
askIndex cn = ctIxImpl <$> askType cn

-- | Throw an error in the Hunt monad.
throwResError :: DocTable dt => Int -> Text -> Hunt dt a
throwResError n msg
    = do errorM $ unwords [show n, T.unpack msg]
         throwError $ ResError n msg

-- ------------------------------------------------------------

-- | Execute the command in the Hunt monad.
execCmd :: (Binary dt, DocTable dt) => Command -> Hunt dt CmdResult
execCmd
  = execBasicCmd . toBasicCommand

-- XXX: kind of obsolete now
-- | Execute the \"low-level\" command in the Hunt monad.

execBasicCmd :: (Binary dt, DocTable dt) => BasicCommand -> Hunt dt CmdResult
execBasicCmd cmd@(InsertList _) = do
  debugM $ "Exec: InsertList [..]"
  execCmd' cmd

execBasicCmd cmd = do
  debugM $ "Exec: " ++ logShow cmd
  execCmd' cmd


-- | Use 'execBasicCmd'.
--
--   Dispatches basic commands to corresponding functions.

execCmd' :: (Binary dt, DocTable dt) => BasicCommand -> Hunt dt CmdResult
execCmd' (Search q offset mx wg fields)
  = withIx $ execSearch q offset mx wg fields

execCmd' (Completion q mx)
    = withIx $ execCompletion q mx
--  = withIx $ execSearch' (wrapCompletion mx) q

execCmd' (Select q)
  = withIx $ execSelect q

execCmd' (Sequence cs)
  = execSequence cs

execCmd' NOOP
  = return ResOK  -- keep alive test

execCmd' (Status sc)
  = execStatus sc

execCmd' (InsertList docs)
  = flushAndMerge False $ execInsertList docs

execCmd' (Update doc)
  = flushAndMerge True $ modIx $ execUpdate doc

execCmd' (DeleteDocs uris)
  = flushAndMerge True $ modIx $ execDeleteDocs uris

execCmd' (DeleteByQuery q)
  = flushAndMerge True $ modIx $ execDeleteByQuery q

execCmd' (StoreIx filename)
  = withIx $ execStore filename

execCmd' (LoadIx filename)
  = execLoad filename

execCmd' (InsertContext cx ct)
  = modIx $ execInsertContext cx ct

execCmd' (DeleteContext cx)
  = modIx $ execDeleteContext cx

execCmd' Snapshot
  = flushAndMerge False execMerge
-- ------------------------------------------------------------

-- | Execute a sequence of commands.
--   The sequence will be aborted if a command fails, but the previous commands will be permanent.

execSequence :: (DocTable dt, Binary dt)=> [BasicCommand] -> Hunt dt CmdResult
execSequence []       = execBasicCmd NOOP
execSequence [c]      = execBasicCmd c
execSequence (c : cs) = execBasicCmd c >> execSequence cs

-- | Insert a context with associated schema.
execInsertContext :: DocTable dt
                  => Context
                  -> ContextSchema
                  -> ContextIndex dt
                  -> Hunt dt (ContextIndex dt, CmdResult)
execInsertContext cx ct ixx
  = do
    -- check if context already exists
    contextExists        <- CIx.hasContextM cx ixx
    unless' (not contextExists)
           409 $ "context already exists: " `T.append` cx

    -- check if type exists in this interpreter instance
    cType                <- askType . ctName . cxType  $ ct
    impl                 <- askIndex . ctName . cxType $ ct
    norms                <- mapM (askNormalizer . cnName) $ cxNormalizer ct
    tokenizer            <- Trav.mapM askTokenizer $ cxTokenizer ct

    -- create new index instance and insert it with context
    return $ ( CIx.insertContext cx (newIx impl) (newSchema cType tokenizer norms) ixx
             , ResOK
             )
  where
  newIx :: IndexImpl -> IndexImpl
  newIx (IndexImpl i) = mkIndex $ Ix.empty `asTypeOf` i
  newSchema cType tok norms= (ct { cxType = cType, cxTokenizer = tok, cxNormalizer = norms })

-- | Deletes the context and the schema associated with it.
execDeleteContext :: DocTable dt
                  => Context
                  -> ContextIndex dt
                  -> Hunt dt (ContextIndex dt, CmdResult)
execDeleteContext cx ixx
  = return (CIx.deleteContext cx ixx, ResOK)

-- | Inserts an 'ApiDocument' into the index.
--
-- /Note/: All contexts mentioned in the 'ApiDocument' need to exist.
-- Documents/URIs must not exist.

execInsertList :: DocTable dt => [ApiDocument] -> Hunt dt CmdResult
execInsertList docs = do
  -- Do the hard work before we take the lock. This enables more parallel indexing
  -- In cases of DocId collisions we do a lot of useless work upfront
  -- TODO: we need to account for concurrent schema changes
  !newSeg <- withIx $ \ixx ->
    lift $ Segment.fromDocsAndWords (CIx.schema ixx) (docsAndWords (CIx.schema ixx))

  modIx $ \ixx' -> do
    -- existence check for all referenced contexts in all docs
    checkContextsExistence contexts ixx'

    -- check no duplicates in docs
    checkDuplicates duplicates

    -- apidoc should not exist
    mapM_ (flip (checkApiDocExistence False) ixx') docs

    -- Inserts the segment
    ixx'' <- CIx.insertSegment newSeg ixx'
    return (ixx'', ResOK)
  where
      -- compute all contexts in all docs
      contexts
          = M.keys
            . M.unions
            . L.map (M.map (const ()) . adIndex)
            $ docs

      -- convert ApiDocuments to Documents, delete null values,
      -- and break index data into words by applying the scanner
      -- given by the schema spec for the appropriate contexts
      docsAndWords schema
           = L.map ( (\ (d, _dw, ws) -> (d, ws))
                    . toDocAndWords schema
                    . (\ d -> d {adDescr = DocDesc.deleteNull $ adDescr d})
                  )
            $ docs

      -- compute duplicate URIs by building a frequency table
      -- and looking for entries with counts @> 1@
      duplicates
          = M.keys
            . M.filter (> 1)
            . L.foldl ins M.empty
            . L.map adUri
            $ docs
            where
              ins m k = M.insertWith (+) k (1::Int) m

      -- check and throw error concerning duplicate URIs
      checkDuplicates xs
          = unless' (L.null xs)
              409 $ "duplicate URIs found in document list:" <> showText xs


-- | Updates an 'ApiDocument'.
--
-- /Note/: All contexts mentioned in the 'ApiDocument' need to exist.
-- Documents/URIs need to exist.

execUpdate :: DocTable dt
           => ApiDocument -> ContextIndex dt -> Hunt dt (ContextIndex dt, CmdResult)

execUpdate doc ixx
    = do checkContextsExistence contexts ixx
         docIdM <- liftIO $ CIx.lookupDocumentByURI (uri docs) ixx
         case docIdM of
          Just docId
            -> do ixx'<- lift $
                         CIx.modifyWithDescription (adWght doc) (desc docs) ws docId ixx
                  return (ixx', ResOK)
          Nothing
            -> throwResError 409 $ "document for update not found: " `T.append` uri docs
  where
      contexts
          = M.keys $ adIndex doc
      (docs, _dw, ws)
          = toDocAndWords (CIx.schema ixx) doc

-- | Test whether the contexts are present and otherwise throw an error.

checkContextsExistence :: DocTable dt
                       => [Context] -> ContextIndex dt -> Hunt dt ()
checkContextsExistence cs ixx
    = do ixxContexts        <- S.fromList <$> CIx.contextsM ixx
         let docContexts     = S.fromList cs
         let invalidContexts = S.difference docContexts ixxContexts
         unless' (S.null invalidContexts)
           409 ( "mentioned context(s) are not present: "
                 <> (showText . S.toList $ invalidContexts)
               )

-- | Test whether the document (URI) already exists
-- (or does not exist depending on the first argument).
--
-- Throws an error if it exists and the first argument is @False@ and vice versa.

checkApiDocExistence :: DocTable dt
                     => Bool -> ApiDocument -> ContextIndex dt -> Hunt dt ()
checkApiDocExistence switch apidoc ixx
    = do let u = adUri apidoc
         mem <- liftIO $ CIx.member u ixx
         unless' (switch == mem)
           409 ( if mem
                   then "document already exists: "
                   else "document does not exist: "
               )

execSearch :: DocTable dt
           => Query
           -> Int
           -> Int
           -> Bool
           -> Maybe [Text]
           -> ContextIndex dt
           -> Hunt dt CmdResult
execSearch q offset mx wg fields ixx
    = do debugM ("execSearch: " ++ show q)
         cfg    <- asks huntQueryCfg
         scDocs <- liftHunt $
                   runQueryScoredDocsM ixx cfg q
         docs   <- liftIO $
                   CIx.selectDocuments (scoredDocsToDocIdSet scDocs) ixx
         formatPage <$>
           toDocsResult (fmap unwrap . flip DocIdMap.lookup docs) scDocs
    where
      formatPage ds
          = ResSearch $
            LimitedResult
            { lrResult = ds'
            , lrOffset = offset
            , lrMax    = mx
            , lrCount  = length ds
            }
          where
            ds' = map (mkSelect wg fields)
                  . toDocumentResultPage offset mx
                  $ ds

execCompletion :: DocTable dt =>
                  Query ->
                  Int ->
                  ContextIndex dt -> Hunt dt CmdResult
execCompletion q mx ixx
    = do debugM ("execCompletion: " ++ show q)
         cfg     <- asks huntQueryCfg
         scWords <- liftHunt $
                    runQueryScoredWordsM ixx cfg q
         return $ ResSuggestion $ toWordsResult mx scWords

execSelect :: DocTable dt => Query -> ContextIndex dt -> Hunt dt CmdResult
execSelect q ixx
    = do debugM ("execSelect: " ++ show q)
         res <- liftHunt $ runQueryUnScoredDocsM ixx queryConfigDocIds q
         dt' <- liftIO $ CIx.selectDocuments (unScoredDocsToDocIdSet res) ixx
         return $ ResGeneric (toJSON (fmap unwrap dt'))

-- | Build a selection function for choosing,
-- which parts of a document are contained in the result.
--
-- The 1. param determines, whether the weight of the document is included in the result.
-- The 2. is the list of the description keys, if @Nothing@ is given the complete desc is included.

mkSelect :: Bool -> Maybe [Text] -> (RankedDoc -> RankedDoc)
mkSelect withWeight fields
    = mkSelW withWeight . mkSelF fields
      where
        mkSelW True      = id
        mkSelW False     = RD . second (\d -> d { wght = 1.0 }) . unRD

        mkSelF Nothing   = id
        mkSelF (Just fs) = RD . second (\d -> d {desc = DocDesc.restrict fs (desc d)}) . unRD


-- | Delete a set of documents.

execDeleteDocs :: DocTable dt => Set URI -> ContextIndex dt -> Hunt dt (ContextIndex dt, CmdResult)
execDeleteDocs d ix
    = do ix' <- lift $ CIx.deleteDocsByURI d ix
         return (ix', ResOK)

-- | Delete all documents matching the query.

execDeleteByQuery :: DocTable dt => Query -> ContextIndex dt -> Hunt dt (ContextIndex dt, CmdResult)
execDeleteByQuery q ixx
    = do debugM ("execDeleteByQuery: " ++ show q)
         ds <- unScoredDocsToDocIdSet <$>
               (liftHunt $ runQueryUnScoredDocsM ixx queryConfigDocIds q)
         if DocIdSet.null ds
           then do debugM "DeleteByQuery: Query result set empty"
                   return (ixx, ResOK)
           else do debugM $ "DeleteByQuery: " ++ show ds
                   ix' <- lift $ CIx.delete ds ixx
                   return (ix', ResOK)

-- ------------------------------------------------------------

-- TODO: catch exceptions:
--       http://hackage.haskell.org/package/base/docs/System-IO.html#v:openFile

-- | Serialize a value to a file.
execStore :: (Binary a, DocTable dt) =>
             FilePath -> a -> Hunt dt CmdResult
execStore filename x = do
  res <- liftIO . tryIOError $ encodeFile filename x
  case res of
      Left  e
          | isAlreadyInUseError e -> throwResError 409 $ "Cannot store index: file is already in use"
          | isPermissionError   e -> throwResError 403 $ "Cannot store index: no access permission to file"
          | isFullError         e -> throwResError 500 $ "Cannot store index: device is full"
          | otherwise             -> throwResError 500 $ showText $ e
      Right _ -> return ResOK

-- TODO: XMVar functions probably not suited for this, locking for load reasonable

-- | Load a context index.
--   The deserialization is more specific because of the existentially typed index.

--   This operation locks the index, otherwise two potentially large indexes could be present at a
--   time. This is still possible if a read operation lasts as long as loading the index.

execLoad :: (Binary dt, DocTable dt) => FilePath -> Hunt dt CmdResult
execLoad _filename = undefined

_execSnapshot :: (Binary dt, DocTable dt) => ContextIndex dt -> Hunt dt (ContextIndex dt, CmdResult)
_execSnapshot ixx
  = do return (ixx, ResOK)

-- ------------------------------------------------------------

-- the query interpreters

-- for scored docs (DocIdMap with scores)

runQueryScoredDocsM :: ContextIndex dt
                    -> ProcessConfig
                    -> Query
                    -> IO (Either CmdError ScoredDocs)
runQueryScoredDocsM ix cfg q
    = processQueryScoredDocs st q
      where
        st = initProcessor cfg (mkQueryIndex ix)

-- for unscored docs (DocIdSet), usually called with 'queryConfigDocIds'

runQueryUnScoredDocsM :: ContextIndex dt
                    -> ProcessConfig
                    -> Query
                    -> IO (Either CmdError UnScoredDocs)
runQueryUnScoredDocsM ix cfg q
    = processQueryUnScoredDocs st q
      where
        st = initProcessor cfg (mkQueryIndex ix)


-- for scored docs (DocIdMap with scores

runQueryScoredWordsM :: ContextIndex dt
                     -> ProcessConfig
                     -> Query
                     -> IO (Either CmdError ScoredWords)
runQueryScoredWordsM ix cfg q
    = processQueryScoredWords st q
      where
        st = initProcessor cfg (mkQueryIndex ix)


-- | Query config for \"delete by query\".

queryConfigDocIds :: ProcessConfig
queryConfigDocIds = ProcessConfig def True 0 0

liftHunt :: IO (Either CmdError r) -> Hunt dt r
liftHunt cmd
    = lift cmd >>= either throwError return

-- ------------------------------------------------------------

execMerge :: DocTable dt => Hunt dt CmdResult
execMerge = return ResOK

-- | Get status information about the server\/index, e.g. garbage collection statistics.
execStatus :: DocTable dt => StatusCmd -> Hunt dt CmdResult
execStatus StatusGC
  = do
    statsEnabled <- liftIO getGCStatsEnabled
    if statsEnabled
      then (ResGeneric . toJSON) <$>
           liftIO getGCStats
      else throwResError 501 ("GC stats not enabled. Use `+RTS -T -RTS' to enable them." :: Text)

execStatus StatusDocTable
    = withIx dumpDocTable
      where
        dumpDocTable _ixx
            = ResGeneric <$> undefined
              -- DocTable.toJSON'DocTable (CIx.docTable ixx)

execStatus (StatusContext cx)
    = withIx dumpContext
      where
        dumpContext ixx
            = (ResGeneric . object . map (uncurry (.=)) . map (second searchResultToOccurrences)) <$>
              liftIO (CIx.lookupAllWithCx cx ixx)

execStatus (StatusIndex {- context -})
  = withIx $ \ixx -> (ResGeneric . toJSON) <$> CIx.status ixx

-- ------------------------------------------------------------

-- | Throw an error unless the first argument is @True@, and otherwise do nothing.
unless' :: DocTable dt
       => Bool -> Int -> Text -> Hunt dt ()
unless' b code text = unless b $ throwResError code text

-- ------------------------------------------------------------


type IndexMerger = Worker

type IndexWorker = Worker

-- | Tickles the flush and merge workers so that the can
--   commit and merge the index if necessary.
flushAndMerge :: DocTable dt => Bool -> Hunt dt a -> Hunt dt a
flushAndMerge delay f = do
  a <- f
  if delay
    then Worker.delayedTickle 500000 =<< asks huntIndexWorker
    else Worker.tickle =<< asks huntIndexWorker
  return a

-- | Create an asynchronous worker for index merging
newIndexMerger :: (MonadIO m, DocTable dt)
               => XMVar (ContextIndex dt)
               -> MergePolicy
               -> m IndexMerger
newIndexMerger xmvar policy = liftIO $ do
  -- we need a lock for parallel merges
  mlock <- newTMVarIO mempty

  Worker.new (mpMaxParallelMerges policy) xmvar $ \_ modify -> do
    -- Take the merge lock, no concurrent access to the section below
    mdesc <- withMergeLock mlock $ \lock -> do
      -- Create the merge descriptions, we need to modify the contextindex here
      -- because it we need to generate new `SegmentId`s for the merged segments.
      modify (\ixx -> do
                  (mdesc, lock', ixx') <- Merge.tryMerge policy lock ixx
                  return (ixx', (mdesc, lock'))
             )

    -- Do the actual merge. Since the result of a merge is a
    -- monoid (and commutative) we could run these in parallel.
    -- We could then have only 1 index merger which distributes
    -- the work to other threads.
    -- We want the work to happen here (on this thread) hence the forcing.
    !merge <- mconcat <$> mapM Merge.runMerge mdesc

    -- Work is done, we need to modify the contextindex and
    -- remove the merged segments from the mergelock
    withMergeLock mlock $ \lock -> do
      lock' <- modify (return . Merge.applyMerge merge lock)
      return ((), lock')

    return ()
  where
    -- This seems to be a pretty rigid construct for the type checker
    -- be careful with the types here.
    withMergeLock :: TMVar Merge.MergeLock
              -> (Merge.MergeLock -> IO (a, Merge.MergeLock))
              -> IO a
    withMergeLock mlock f = do
      lock <- atomically (takeTMVar mlock)
      (a, !lock') <- f lock
      atomically (putTMVar mlock lock')
      return a

type IndexFlusher = Worker

-- | Flushes changes to the index to disk.
newIndexFlusher :: (MonadIO m, DocTable dt)
                => XMVar (ContextIndex dt)
                -> FlushPolicy
                -> m IndexFlusher
newIndexFlusher xmvar policy  = liftIO $ do
  mLastRev <- newIORef =<< Flush.mkRevision =<< readXMVar xmvar
  -- Disallow concurrent flushing by having exactly one worker.
  Worker.new 1 xmvar $ \readix modify -> do
    lastRev <- readIORef mLastRev
    ixx     <- readix

    -- Diff the current index to the last one
    (doFlush, newRev) <- Flush.delta lastRev ixx

    -- Run the resulting flush,
    -- runFlush is able to produce some index modifiying
    -- function, e.g. as it stores all documents on disk
    -- the function can replace the expensive in-memory
    -- DocTable with a cheaper one reading directly from disk.
    fls <- Flush.runFlush policy doFlush
    modify (return . Flush.applyFlush fls)

    -- Store the revision for the next iteration.
    writeIORef mLastRev newRev
