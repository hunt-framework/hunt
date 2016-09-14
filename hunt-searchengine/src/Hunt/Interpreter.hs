{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
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
import           Control.Concurrent.XMVar
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson                    (ToJSON (..), object, (.=))
import           Data.Binary                   (Binary, encodeFile)
import qualified Data.ByteString.Lazy          as BL
import           Data.Default
import qualified Data.List                     as L
import qualified Data.Map                      as M
import           Data.Monoid                   ((<>))
import           Data.Set                      (Set)
import qualified Data.Set                      as S
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Traversable              as TV
import           GHC.Stats                     (getGCStats, getGCStatsEnabled)
import           GHC.Stats.Json                ()
import           Hunt.Common.ApiDocument       as ApiDoc
import           Hunt.Common.BasicTypes        (Context, URI)
import qualified Hunt.Common.DocDesc           as DocDesc
import qualified Hunt.Common.DocIdSet          as DocIdSet
import           Hunt.Common.Document          (Document (..))
import           Hunt.ContextIndex             (ContextIndex (..), ContextMap)
import qualified Hunt.ContextIndex             as CIx
import           Hunt.DocTable                 (DocTable)
import qualified Hunt.DocTable                 as DocTable
import           Hunt.DocTable.HashedDocTable
import qualified Hunt.Index                    as Ix
import           Hunt.Index.IndexImpl          (IndexImpl (..), mkIndex)
import           Hunt.Index.Schema
import           Hunt.Index.Schema.Analyze
import           Hunt.Interpreter.BasicCommand
import           Hunt.Interpreter.Command      (Command)
import           Hunt.Interpreter.Command      hiding (Command (..))
import           Hunt.Query.Intermediate       (RankedDoc (..), ScoredWords,
                                                toDocsResult,
                                                toDocumentResultPage,
                                                toWordsResult)
import           Hunt.Query.Language.Grammar
import           Hunt.Query.Processor          (ProcessConfig (..),
                                                initProcessor,
                                                processQueryScoredDocs,
                                                processQueryScoredWords,
                                                processQueryUnScoredDocs)
import           Hunt.Scoring.SearchResult     (ScoredDocs, UnScoredDocs,
                                                searchResultToOccurrences,
                                                unScoredDocsToDocIdSet)
import qualified Hunt.SegmentIndex.Commit      as Commit
import           Hunt.Utility                  (showText)
import           Hunt.Utility.Log
import           System.IO.Error               (isAlreadyInUseError,
                                                isDoesNotExistError,
                                                isFullError, isPermissionError,
                                                tryIOError)
import qualified System.Log.Logger             as Log

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
    huntIndex       :: XMVar (ContextIndex)
    -- | Available context types.
  , huntTypes       :: ContextTypes
    -- | Available normalizers.
  , huntNormalizers :: [CNormalizer]
    -- | Query processor configuration.
  , huntQueryCfg    :: ProcessConfig
  }

-- | Default Hunt environment type.
type DefHuntEnv = HuntEnv (Documents Document)

-- | Initialize the Hunt environment with default values.
initHunt :: IO (HuntEnv dt)
initHunt = initHuntEnv CIx.empty contextTypes normalizers def

-- | Default context types.
contextTypes :: ContextTypes
contextTypes = [ctText, ctInt, ctDate, ctPosition, ctTextSimple, ctPositionRTree]

-- | Default normalizers.
normalizers :: [CNormalizer]
normalizers = [cnUpperCase, cnLowerCase, cnZeroFill]

-- | Initialize the Hunt environment.
initHuntEnv :: ContextIndex
           -> ContextTypes
           -> [CNormalizer]
           -> ProcessConfig
           -> IO (HuntEnv dt)
initHuntEnv ixx opt ns qc = do
  ixref <- newXMVar ixx
  return $ HuntEnv ixref opt ns qc

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

runHunt :: HuntT dt m a -> HuntEnv dt -> m (Either CmdError a)
runHunt env = runExceptT . runReaderT (runHuntT env)

-- | Run the command the supplied environment/state.
runCmd :: HuntEnv dt -> Command -> IO (Either CmdError CmdResult)
runCmd env cmd
  = runExceptT . runReaderT (runHuntT . execCmd $ cmd) $ env

-- | Get the context index.
askIx :: Hunt dt (ContextIndex)
askIx = do
  ref <- asks huntIndex
  liftIO $ readXMVar ref

-- FIXME: io exception-safe?
-- | Modify the context index.
modIx :: (ContextIndex -> Hunt dt (ContextIndex, a)) -> Hunt dt a
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
modIxLocked :: (ContextIndex -> Hunt dt (ContextIndex, a)) -> Hunt dt a
modIxLocked f = do
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
withIx :: (ContextIndex -> Hunt dt a) -> Hunt dt a
withIx f
  = askIx >>= f

-- | Get the type of a context.
askType :: Text -> Hunt dt ContextType
askType cn = do
  ts <- asks huntTypes
  case L.find (\t -> cn == ctName t) ts of
    Just t -> return t
    _      -> throwResError 410 ("used unavailable context type: " `T.append` cn)

-- | Get the normalizer of a context.
askNormalizer :: Text -> Hunt dt CNormalizer
askNormalizer cn = do
  ts <- asks huntNormalizers
  case L.find (\t -> cn == cnName t) ts of
    Just t -> return t
    _      -> throwResError 410 ("used unavailable normalizer: " `T.append` cn)

-- | Get the index.
askIndex :: Text -> Hunt dt IndexImpl
askIndex cn = ctIxImpl <$> askType cn

-- | Throw an error in the Hunt monad.
throwResError :: Int -> Text -> Hunt dt a
throwResError n msg
    = do errorM $ unwords [show n, T.unpack msg]
         throwError $ ResError n msg

-- ------------------------------------------------------------

-- | Execute the command in the Hunt monad.
execCmd :: Command -> Hunt dt CmdResult
execCmd
  = execBasicCmd . toBasicCommand

-- XXX: kind of obsolete now
-- | Execute the \"low-level\" command in the Hunt monad.

execBasicCmd :: BasicCommand -> Hunt dt CmdResult
execBasicCmd cmd@(InsertList _) = do
  debugM $ "Exec: InsertList [..]"
  execCmd' cmd

execBasicCmd cmd = do
  debugM $ "Exec: " ++ logShow cmd
  execCmd' cmd


-- | Use 'execBasicCmd'.
--
--   Dispatches basic commands to corresponding functions.

execCmd' :: BasicCommand -> Hunt dt CmdResult
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
  = modIx $ execInsertList docs

execCmd' (Update doc)
  = modIx $ execUpdate doc

execCmd' (DeleteDocs uris)
  = modIx $ execDeleteDocs uris

execCmd' (DeleteByQuery q)
  = modIx $ execDeleteByQuery q

execCmd' (StoreIx filename)
  = withIx $ \ixx -> do liftIO $ Commit.writeIndex "./" (cxToCxNum ixx) 0 (CIx.indexedWords ixx)
                        return ResOK
                        --execStore filename ixx

  where
    cxToCxNum ixx cx = M.findIndex cx (CIx.mapToSchema ixx)

execCmd' (LoadIx filename)
  = execLoad filename

execCmd' (InsertContext cx ct)
  = modIx $ execInsertContext cx ct

execCmd' (DeleteContext cx)
  = modIx $ execDeleteContext cx

-- ------------------------------------------------------------

-- | Execute a sequence of commands.
--   The sequence will be aborted if a command fails, but the previous commands will be permanent.

execSequence :: [BasicCommand] -> Hunt dt CmdResult
execSequence []       = execBasicCmd NOOP
execSequence [c]      = execBasicCmd c
execSequence (c : cs) = execBasicCmd c >> execSequence cs

-- | Insert a context with associated schema.
execInsertContext :: Context
                  -> ContextSchema
                  -> ContextIndex
                  -> Hunt dt (ContextIndex, CmdResult)
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

    -- create new index instance and insert it with context
    return $ ( CIx.insertContext cx (newIx impl) (newSchema cType norms) ixx
             , ResOK
             )
  where
  newIx :: IndexImpl -> IndexImpl
  newIx (IndexImpl i) = mkIndex $ Ix.empty `asTypeOf` i
  newSchema cType norms= (ct { cxType = cType, cxNormalizer = norms })

-- | Deletes the context and the schema associated with it.
execDeleteContext :: Context
                  -> ContextIndex
                  -> Hunt dt (ContextIndex, CmdResult)
execDeleteContext cx ixx
  = return (CIx.deleteContext cx ixx, ResOK)

-- | Inserts an 'ApiDocument' into the index.
--
-- /Note/: All contexts mentioned in the 'ApiDocument' need to exist.
-- Documents/URIs must not exist.

execInsertList :: [ApiDocument] -> ContextIndex -> Hunt dt (ContextIndex, CmdResult)
execInsertList docs ixx
    = do -- existence check for all referenced contexts in all docs
         checkContextsExistence contexts ixx

         -- check no duplicates in docs
         checkDuplicates duplicates

         -- apidoc should not exist
         mapM_ (flip (checkApiDocExistence False) ixx) docs

         -- all checks done, do the real work
         ixx' <- lift $ CIx.insertList docsAndWords ixx
         return (ixx', ResOK)
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
      docsAndWords
          = L.map ( (\ (d, _dw, ws) -> (d, ws))
                    . toDocAndWords (CIx.mapToSchema ixx)
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

execUpdate :: ApiDocument -> ContextIndex -> Hunt dt (ContextIndex, CmdResult)

execUpdate doc ixx
    = do checkContextsExistence contexts ixx
         docIdM <- lift $ CIx.lookupDocumentByURI ixx (uri docs)
         case docIdM of
           Just docId
               -> do ixx' <- lift
                             $ CIx.modifyWithDescription (adWght doc) (desc docs) ws docId ixx
                     return (ixx', ResOK)
           Nothing
               -> throwResError 409 $ "document for update not found: " `T.append` uri docs
    where
      contexts
          = M.keys $ adIndex doc
      (docs, _dw, ws)
          = toDocAndWords (CIx.mapToSchema ixx) doc


-- | Test whether the contexts are present and otherwise throw an error.

checkContextsExistence :: [Context] -> ContextIndex -> Hunt dt ()
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

checkApiDocExistence :: Bool -> ApiDocument -> ContextIndex -> Hunt dt ()
checkApiDocExistence switch apidoc ixx
    = do let u = adUri apidoc
         mem <- CIx.member u ixx
         unless' (switch == mem)
           409 ( ( if mem
                   then "document already exists: "
                   else "document does not exist: "
                 ) <> u
               )

execSearch :: Query ->
              Int -> Int ->
              Bool -> Maybe [Text] ->
              ContextIndex ->
              Hunt dt CmdResult

execSearch q offset mx wg fields ixx
    = do debugM ("execSearch: " ++ show q)
         cfg    <- asks huntQueryCfg
         scDocs <- liftHunt $
                   runQueryScoredDocsM ixx cfg q
         formatPage <$> toDocsResult (CIx.lookupDocument ixx) scDocs
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

execCompletion :: Query ->
                  Int ->
                  ContextIndex -> Hunt dt CmdResult
execCompletion q mx ix
    = do debugM ("execCompletion: " ++ show q)
         cfg     <- asks huntQueryCfg
         scWords <- liftHunt $
                    runQueryScoredWordsM ix cfg q
         return $ ResSuggestion $ toWordsResult mx scWords


execSelect :: Query -> ContextIndex -> Hunt dt CmdResult
execSelect q ixx
    = do debugM ("execSelect: " ++ show q)
         res <- liftHunt $ runQueryUnScoredDocsM ixx queryConfigDocIds q
         docs <- CIx.lookupDocuments ixx (unScoredDocsToDocIdSet res)
         return $ ResGeneric (toJSON docs)

-- | Build a selection function for choosing,
-- which parts of a document are contained in the result.
--
-- The 1. param determines, whether the weight of the document is included in the result.
-- The 2. is the list of the description keys, if @Nothing@ is given the complete desc is included.

mkSelect :: Bool -> Maybe [Text] -> (RankedDoc -> RankedDoc)
mkSelect withWeight fields
    = mkSelW withWeight . mkSelF fields
      where
        mkSelW True  = id
        mkSelW False = RD . second (\d -> d { wght = 1.0 }) . unRD

        mkSelF Nothing   = id
        mkSelF (Just fs) = RD . second (\d -> d {desc = DocDesc.restrict fs (desc d)}) . unRD


-- | Delete a set of documents.

execDeleteDocs :: Set URI -> ContextIndex -> Hunt dt (ContextIndex, CmdResult)
execDeleteDocs d ix
    = do ix' <- lift $ CIx.deleteDocsByURI d ix
         return (ix', ResOK)

-- | Delete all documents matching the query.

execDeleteByQuery :: Query -> ContextIndex -> Hunt dt (ContextIndex, CmdResult)
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
execStore :: Binary a => FilePath -> a -> Hunt dt CmdResult
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

execLoad :: FilePath -> Hunt dt CmdResult
execLoad filename = do
  ts <- asks huntTypes
  let ix = map ctIxImpl ts
  modIxLocked $ \_ -> do
    ixx <- decodeFile' ix filename
    return (ixx, ResOK)
  where
  decodeFile' ts f = do
    res <- liftIO . tryIOError $ BL.readFile f
    case res of
      Left  e
          | isAlreadyInUseError e -> throwResError 409 $ "Cannot load index: file already in use"
          | isDoesNotExistError e -> throwResError 404 $ "Cannot load index: file does not exist"
          | isPermissionError   e -> throwResError 403 $ "Cannot load index: no access permission to file"
          | otherwise             -> throwResError 500 $ showText e
      Right r -> CIx.loadCxIx ts askType askNormalizer r

-- ------------------------------------------------------------

-- the query interpreters

-- for scored docs (DocIdMap with scores)

runQueryScoredDocsM :: ContextIndex
                    -> ProcessConfig
                    -> Query
                    -> IO (Either CmdError ScoredDocs)
runQueryScoredDocsM ix cfg q
    = processQueryScoredDocs st q
      where
        st = initProcessor cfg ix


-- for unscored docs (DocIdSet), usually called with 'queryConfigDocIds'

runQueryUnScoredDocsM :: ContextIndex
                      -> ProcessConfig
                      -> Query
                      -> IO (Either CmdError UnScoredDocs)
runQueryUnScoredDocsM ix cfg q
    = processQueryUnScoredDocs st q
      where
        st = initProcessor cfg ix


-- for scored docs (DocIdMap with scores

runQueryScoredWordsM :: ContextIndex
                     -> ProcessConfig
                     -> Query

                     -> IO (Either CmdError ScoredWords)
runQueryScoredWordsM ix cfg q
    = processQueryScoredWords st q
      where
        st = initProcessor cfg ix


-- | Query config for \"delete by query\".

queryConfigDocIds :: ProcessConfig
queryConfigDocIds = ProcessConfig def True 0 0

liftHunt :: IO (Either CmdError r) -> Hunt dt r
liftHunt cmd
    = lift cmd >>= either throwError return

-- ------------------------------------------------------------

-- | Get status information about the server\/index, e.g. garbage collection statistics.
execStatus :: StatusCmd -> Hunt dt CmdResult
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
        dumpDocTable ixx
            = ResGeneric <$> undefined

execStatus (StatusContext cx)
    = withIx dumpContext
      where
        dumpContext ixx = undefined
--            = (ResGeneric . object . map (uncurry (.=)) . map (second searchResultToOccurrences)) <$>
--              CIx.lookupAllWithCx cx ix

execStatus (StatusIndex {- context -})
  = withIx _dumpIndex
    where
      -- context = "type"
      _dumpIndex ixx
        = do let words = CIx.indexedWords ixx
             return $ ResGeneric (toJSON words)

-- ------------------------------------------------------------

-- | Throw an error unless the first argument is @True@, and otherwise do nothing.
unless' :: Bool -> Int -> Text -> Hunt dt ()
unless' b code text = unless b $ throwResError code text

-- ------------------------------------------------------------
