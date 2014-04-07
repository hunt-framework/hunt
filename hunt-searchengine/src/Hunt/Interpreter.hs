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

import           Control.Applicative
import           Control.Concurrent.XMVar
import           Control.Monad.Error
import           Control.Monad.Reader

import qualified Data.Aeson                            as JS
import qualified Data.Binary                           as Bin
import qualified Data.ByteString.Lazy                  as BL
import           Data.Default
import           Data.Function                         (on)
import qualified Data.IntSet                           as IS
import           Data.List                             (sortBy)
import qualified Data.List                             as L
import qualified Data.Map                              as M
import           Data.Set                              (Set)
import qualified Data.Set                              as S
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import qualified Data.Traversable                      as TV

import           Hunt.Common
import           Hunt.Common.ApiDocument               as ApiDoc
import qualified Hunt.Common.DocIdMap                  as DM
import           Hunt.Common.Document                  (DocumentWrapper, unwrap)

import qualified Hunt.Index                            as Ix
import           Hunt.Index.Schema.Analyze

import           Hunt.ContextIndex                     (ContextIndex (..),
                                                        decodeCxIx)
import qualified Hunt.ContextIndex                     as Ixx

import           Hunt.Index.IndexImpl                  (IndexImpl (..), mkIndex)
import qualified Hunt.Index.IndexImpl                  as Impl

import           Hunt.Query.Language.Grammar
import           Hunt.Query.Processor
import qualified Hunt.Query.Processor                  as QProc
import           Hunt.Query.Ranking
import           Hunt.Query.Result                     as QRes

import           Hunt.DocTable                         (DocTable)
import qualified Hunt.DocTable                         as Dt
import           Hunt.DocTable.HashedDocTable

import           Hunt.Interpreter.BasicCommand
import           Hunt.Interpreter.Command              (Command)
import           Hunt.Interpreter.Command              hiding (Command (..))

import qualified System.Log.Logger                     as Log

import           Hunt.Utility
import           Hunt.Utility.Log

import           GHC.Stats
import           GHC.Stats.Json                        ()

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
    -- | Ranking configuration.
  , huntRankingCfg  :: RankConfig (Dt.DValue dt)
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
initHunt :: DocTable dt => IO (HuntEnv dt)
initHunt = initHuntEnv (ContextIndex Ixx.empty Dt.empty M.empty) defaultRankConfig contextTypes normalizers def

-- | Default context types.
contextTypes :: ContextTypes
contextTypes = [ctText, ctInt, ctDate, ctPosition]

-- | Default normalizers.
normalizers :: [CNormalizer]
normalizers = [cnUpperCase, cnLowerCase, cnZeroFill]

-- | Initialize the Hunt environment.
initHuntEnv :: DocTable dt
           => ContextIndex dt
           -> RankConfig (Dt.DValue dt)
           -> ContextTypes
           -> [CNormalizer]
           -> ProcessConfig
           -> IO (HuntEnv dt)
initHuntEnv ixx rnk opt ns qc = do
  ixref <- newXMVar ixx
  return $ HuntEnv ixref rnk opt ns qc

-- ------------------------------------------------------------
-- Command evaluation monad
-- ------------------------------------------------------------

-- | The Hunt transformer monad. Allows a custom monad to be embedded to combine with other DSLs.
newtype HuntT dt m a
  = HuntT { runHuntT :: ReaderT (HuntEnv dt) (ErrorT CmdError m) a }
  deriving (Applicative, Monad, MonadIO, Functor, MonadReader (HuntEnv dt), MonadError CmdError)

instance MonadTrans (HuntT dt) where
  lift = HuntT . lift . lift

-- | The Hunt monad on 'IO'.
type Hunt dt = HuntT dt IO

-- ------------------------------------------------------------

-- | Run the Hunt monad with the supplied environment/state.
runHunt :: DocTable dt => HuntT dt m a -> HuntEnv dt -> m (Either CmdError a)
runHunt env = runErrorT . runReaderT (runHuntT env)

-- | Run the command the supplied environment/state.
runCmd :: (DocTable dt, Bin.Binary dt) => HuntEnv dt -> Command -> IO (Either CmdError CmdResult)
runCmd env cmd
  = runErrorT . runReaderT (runHuntT . execCmd $ cmd) $ env

-- | Get the context index.
askIx :: DocTable dt => Hunt dt (ContextIndex dt)
askIx = do
  ref <- asks huntIndex
  liftIO $ readXMVar ref

-- FIXME: io exception-safe?
-- | Modify the context index.
modIx :: DocTable dt
      => (ContextIndex dt-> Hunt dt (ContextIndex dt, a)) -> Hunt dt a
modIx f = do
  ref <- asks huntIndex
  ix <- liftIO $ takeXMVarWrite ref
  (i',a) <- f ix `catchError` putBack ref ix
  liftIO $ putXMVarWrite ref i'
  return a
  where
  putBack ref i e = do
    liftIO $ putXMVarWrite ref i
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

-- | Get the index.
askIndex :: DocTable dt => Text -> Hunt dt (Impl.IndexImpl Occurrences)
askIndex cn = askType cn >>= return . ctIxImpl
{-
askDocTable :: DocTable dt => Hunt dt dt
askDocTable = askIx >>= return . ciDocs
-}
-- | Get the context weightings.
askContextsWeights :: DocTable dt => Hunt dt(M.Map Context Weight)
askContextsWeights
  = withIx (\(ContextIndex _ _ schema) -> return $ M.map cxWeight schema)

-- | Throw an error in the Hunt monad.
throwResError :: DocTable dt => Int -> Text -> Hunt dt a
throwResError n msg
    = do errorM $ unwords [show n, T.unpack msg]
         throwError $ ResError n msg

-- ------------------------------------------------------------

-- | Execute the command in the Hunt monad.
execCmd :: (Bin.Binary dt, DocTable dt) => Command -> Hunt dt CmdResult
execCmd
  = execBasicCmd . toBasicCommand

-- XXX: kind of obsolete now
-- | Execute the \"low-level\" command in the Hunt monad.
execBasicCmd :: (Bin.Binary dt, DocTable dt) => BasicCommand -> Hunt dt CmdResult
execBasicCmd cmd@(InsertList _) = do
  debugM $ "Exec: InsertList [..]"
  execCmd' cmd
execBasicCmd cmd = do
  debugM $ "Exec: " ++ logShow cmd
  execCmd' cmd

-- | Use 'execBasicCmd'.
--
--   Dispatches basic commands to corresponding functions.
execCmd' :: (Bin.Binary dt, DocTable dt) => BasicCommand -> Hunt dt CmdResult
execCmd' (Search q offset mx)
  = withIx $ execSearch' (wrapSearch offset mx) q

execCmd' (Completion q mx)
  = withIx $ execSearch' (wrapCompletion mx) q

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
  = withIx $ execStore filename

execCmd' (LoadIx filename)
  = modIx $ \_ix -> execLoad filename

execCmd' (InsertContext cx ct)
  = modIx $ execInsertContext cx ct

execCmd' (DeleteContext cx)
  = modIx $ execDeleteContext cx

-- ------------------------------------------------------------

-- | Execute a sequence of commands.
--   The sequence will be aborted if a command fails, but the previous commands will be permanent.
execSequence :: (DocTable dt, Bin.Binary dt)=> [BasicCommand] -> Hunt dt CmdResult
execSequence []       = execBasicCmd NOOP
execSequence [c]      = execBasicCmd c
execSequence (c : cs) = execBasicCmd c >> execSequence cs

-- | Insert a context with associated schema.
execInsertContext :: DocTable dt
                  => Context
                  -> ContextSchema
                  -> ContextIndex dt
                  -> Hunt dt (ContextIndex dt, CmdResult)
execInsertContext cx ct ixx@(ContextIndex ix dt s)
  = do
    -- check if context already exists
    contextExists        <- Ixx.hasContext cx ixx
    unless' (not contextExists)
           409 $ "context already exists: " `T.append` cx

    -- check if type exists in this interpreter instance
    cType                <- askType . ctName . cxType  $ ct
    impl                 <- askIndex . ctName . cxType $ ct
    norms                <- mapM (askNormalizer . cnName) $ cxNormalizer ct

    -- create new index instance and insert it with context
    return ( ContextIndex { ciIndex = Ixx.insertContext cx (newIx impl) ix
                 , ciDocs   = dt
                 , ciSchema = M.insert cx (ct
                                            { cxType = cType
                                            , cxNormalizer = norms
                                            }) s
                 }
           , ResOK )
  where
  newIx :: IndexImpl Occurrences -> IndexImpl Occurrences
  newIx (IndexImpl i) = mkIndex $ Ix.empty `asTypeOf` i

-- | Deletes the context and the schema associated with it.
execDeleteContext :: DocTable dt
                  => Context
                  -> ContextIndex dt
                  -> Hunt dt (ContextIndex dt, CmdResult)
execDeleteContext cx ixx
  = return (Ixx.deleteContext cx ixx, ResOK)

-- | Inserts an 'ApiDocument' into the index.
--
-- /Note/: All contexts mentioned in the 'ApiDocument' need to exist.
-- Documents/URIs must not exist.
execInsertList :: DocTable dt
                => [ApiDocument] -> ContextIndex dt -> Hunt dt (ContextIndex dt, CmdResult)
execInsertList docs ixx@(ContextIndex _ix _dt schema) = do
  -- TODO: use set for undup
  let contexts = concatMap (M.keys . adIndex) docs
  checkContextsExistence contexts ixx
  -- apidoc should not exist
  mapM_ (flip (checkApiDocExistence False) ixx) docs
  let docsAndWords = map ((\(d,_dw,ws) -> (d,ws)) . toDocAndWords schema) docs
  ixx' <- lift $ Ixx.insertList docsAndWords ixx
  return (ixx', ResOK)


-- | Updates an 'ApiDocument'.
--
-- /Note/: All contexts mentioned in the 'ApiDocument' need to exist.
-- Documents/URIs need to exist.
execUpdate :: DocTable dt
           => ApiDocument -> ContextIndex dt -> Hunt dt(ContextIndex dt, CmdResult)
execUpdate doc ixx@(ContextIndex _ix dt schema) = do
  let contexts = M.keys $ adIndex doc
  checkContextsExistence contexts ixx
  let (docs, _dw, ws) = toDocAndWords schema doc
  docIdM <- lift $ Dt.lookupByURI (uri docs) dt
  case docIdM of
    Just docId -> do
      ixx' <- lift $ Ixx.modifyWithDescription (adWght doc) (desc docs) ws docId ixx
      return (ixx', ResOK)
    Nothing    ->
      throwResError 409 $ "document for update not found: " `T.append` uri docs

-- | Test whether the contexts are present and otherwise throw an error.
checkContextsExistence :: DocTable dt
                       => [Context] -> ContextIndex dt -> Hunt dt ()
checkContextsExistence cs ixx = do
  ixxContexts        <- S.fromList <$> Ixx.contexts ixx
  let docContexts     = S.fromList cs
  let invalidContexts = S.difference docContexts ixxContexts
  unless' (S.null invalidContexts)
    409 $ "mentioned context(s) are not present: "
            `T.append` (T.pack . show . S.toList) invalidContexts

-- | Test whether the document (URI) already exists (or does not exist depending on the first argument).
--   Throws an error if it exists and the first argument is @False@ and vice versa.
checkApiDocExistence :: DocTable dt
                     => Bool -> ApiDocument -> ContextIndex dt -> Hunt dt ()
checkApiDocExistence switch apidoc ixx = do
  let u = adUri apidoc
  mem <- Ixx.member u ixx
  unless' (switch == mem)
    409 $ (if mem
            then "document already exists: "
            else "document does not exist: ") `T.append` u

-- | Search the index.
--   Requires a result transformation function, e.g. 'wrapSearch' or 'wrapCompletion'.
execSearch' :: (DocTable dt, e ~ Dt.DValue dt)
            => (Result e -> CmdResult)
            -> Query
            -> ContextIndex dt
            -> Hunt dt CmdResult
execSearch' f q (ContextIndex ix dt s)
  = do
    cfg <- asks huntQueryCfg
    r   <- lift $ runQueryM ix s cfg dt q
    rc  <- asks huntRankingCfg
    cw  <- askContextsWeights
    case r of
      Left  err -> throwError err
      Right res -> (f <$>) . rank rc dt cw $ res

-- FIXME: signature to result
-- | Wrap the query result for search.
wrapSearch :: (DocumentWrapper e) => Int -> Int -> Result e -> CmdResult
wrapSearch offset mx
  = ResSearch
    . mkLimitedResult offset mx
--    . map fst -- remove score from result
    . sortBy (descending `on` snd) -- sort by score
    . map (\(_did, (di, _dch)) -> (unwrap . document $ di, docScore di))
    . DM.toList
    . docHits

-- | Wrap the query result for auto-completion.
wrapCompletion :: Int -> Result e -> CmdResult
wrapCompletion mx
  = ResCompletion
    . take mx
    . map (\(word,_score,terms') -> (word, terms')) -- remove score from result
    . sortBy (descending `on` (\(_,score,_) -> score)) -- sort by score
    . map (\(c, (wi, _wch)) -> (c, wordScore wi, terms wi))
    . M.toList
    . wordHits


-- | Delete a set of documents.
execDeleteDocs :: DocTable dt => Set URI -> ContextIndex dt -> Hunt dt(ContextIndex dt, CmdResult)
execDeleteDocs d ix = do
  ix' <- lift $ Ixx.deleteDocsByURI d ix
  return (ix', ResOK)

-- | Delete all documents matching the query.
execDeleteByQuery :: DocTable dt => Query -> ContextIndex dt -> Hunt dt(ContextIndex dt, CmdResult)
execDeleteByQuery q ixx@(ContextIndex ix _dt s) = do
  r <- lift $ runQueryDocIdsM ix s q
  case r of
    Left  err -> throwError err
    Right res ->
        if IS.null res
        then do
          debugM "DeleteByQuery: Query result set empty"
          return (ixx, ResOK)
        else do
          ix' <- lift $ Ixx.delete res ixx
          return (ix', ResOK)

-- ------------------------------------------------------------

-- TODO: catch exceptions:
--       http://hackage.haskell.org/package/base/docs/System-IO.html#v:openFile

-- | Serialize a value to a file.
execStore :: (Bin.Binary a, DocTable dt) =>
             FilePath -> a -> Hunt dt CmdResult
execStore filename x = do
  liftIO $ Bin.encodeFile filename x
  return ResOK

-- TODO: XMVar functions probably not suited for this, locking for load reasonable

-- | Load a context index.
--
--   This is deserialization needs to be more specific because of our existential typed index
execLoad :: (Bin.Binary dt, DocTable dt) => FilePath -> Hunt dt (ContextIndex dt, CmdResult)
execLoad filename = do
  ts <- asks huntTypes
  let ix = map ctIxImpl ts
  ixh@(ContextIndex _ _ s) <- liftIO $ decodeFile' ix filename
  ls <- TV.mapM reloadSchema s
  return (ixh{ ciSchema = ls }, ResOK)
  where
  decodeFile' ts f = do
    bs <- BL.readFile f
    return $ decodeCxIx ts bs

  reloadSchema s = (askType . ctName . cxType) s >>= \t -> return $ s { cxType = t }

-- ------------------------------------------------------------

-- | Run a query.
runQueryM       :: DocTable dt
                => Ixx.ContextMap Occurrences
                -> Schema
                -> ProcessConfig
                -> dt
                -> Query
                -> IO (Either CmdError (QRes.Result (Dt.DValue dt)))
runQueryM ix s cfg dt q = processQuery st dt q
  where
  st = QProc.initProcessor cfg ix s

-- | Query config for \"delete by query\".
queryConfigDocIds :: ProcessConfig
queryConfigDocIds = ProcessConfig def True 0 0

runQueryDocIdsM :: Ixx.ContextMap Occurrences
                -> Schema
                -> Query
                -> IO (Either CmdError DocIdSet)
runQueryDocIdsM ix s q = processQueryDocIds st q
  where
  st = QProc.initProcessor queryConfigDocIds ix s

-- ------------------------------------------------------------

-- | Get status information about the server\/index, e.g. garbage collection statistics.
execStatus :: DocTable dt => StatusCmd -> Hunt dt CmdResult
execStatus StatusGC
  = do
    statsEnabled <- liftIO getGCStatsEnabled
    if statsEnabled
      then liftIO getGCStats >>= return . ResGeneric . JS.toJSON
      else throwResError 501 ("GC stats not enabled. Use `+RTS -T -RTS' to enable them." :: Text)

execStatus StatusDocTable
    = withIx dumpDocTable
      where
        dumpDocTable (ContextIndex _ix dt _s)
            = ResGeneric <$> Dt.toJSON'DocTable dt

execStatus StatusIndex
  = withIx
    (\_ix -> return $ ResGeneric $ JS.String "status of Index not yet implemented" )

-- ------------------------------------------------------------

-- | Throw an error unless the first argument is @True@, and otherwise do nothing.
unless' :: DocTable dt
       => Bool -> Int -> Text -> Hunt dt ()
unless' b code text = unless b $ throwResError code text

-- ------------------------------------------------------------
