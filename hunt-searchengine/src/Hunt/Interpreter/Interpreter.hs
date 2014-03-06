{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Hunt.Interpreter.Interpreter
    ( initHunt
    , runCmd
    , runHunt
    , execCmd
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
import           Data.Function                         (on)
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
import           Hunt.Common.Document.Compression.BZip (CompressedDoc)

import qualified Hunt.Index.Index                      as Ix
import           Hunt.Index.Schema.Analyze

import           Hunt.ContextIndex                     (ContextIndex (..),
                                                        decodeCxIx)
import qualified Hunt.ContextIndex                     as Ixx

import           Hunt.Index.IndexImpl                  (IndexImpl (..), mkIndex)
import qualified Hunt.Index.IndexImpl                  as Impl

import           Hunt.Query.Fuzzy
import           Hunt.Query.Language.Grammar
import           Hunt.Query.Processor
import qualified Hunt.Query.Processor                  as QProc
import           Hunt.Query.Ranking
import           Hunt.Query.Result                     as QRes

import           Hunt.DocTable.DocTable                (DocTable)
import qualified Hunt.DocTable.DocTable                as Dt
import           Hunt.DocTable.HashedDocTable

import           Hunt.Interpreter.BasicCommand
import           Hunt.Interpreter.Command              (Command)
import           Hunt.Interpreter.Command              hiding (Command (..))

import qualified System.Log.Logger                     as Log

import           Hunt.Utility.Log

import           GHC.Stats
import           GHC.Stats.Json                        ()

-- ----------------------------------------------------------------------------
--
-- the semantic domains (datatypes for interpretation)
--
-- HuntEnv, Index, ...

-- ----------------------------------------------------------------------------
--
-- the indexer used in the interpreter
-- this should be a generic interpreter in the end
-- but right now its okay to have the indexer
-- replaceable by a type declaration

-- ----------------------------------------------------------------------------
-- Logging

-- TODO: manage exports

-- | Name of the module for logging purposes.
modName :: String
modName = "Hunt.Interpreter.Interpreter"

-- | Log a message at 'DEBUG' priority.
debugM :: MonadIO m => String -> m ()
debugM = liftIO . Log.debugM modName

-- | Log a message at 'WARNING' priority.
warningM :: MonadIO m => String -> m ()
warningM = liftIO . Log.warningM modName

-- | Log a message at 'ERROR' priority.
errorM :: MonadIO m => String -> m ()
errorM = liftIO . Log.errorM modName

{-
-- | Log formated values that get inserted into a context
debugContext :: Context -> Words -> IO ()
debugContext c ws = debugM $ concat ["insert in ", T.unpack c, show . M.toList $ fromMaybe M.empty $ M.lookup c ws]
-- -}

-- ----------------------------------------------------------------------------
--

data HuntEnv dt = HuntEnv
  { huntIndex       :: DocTable dt => XMVar (ContextIndex dt)
  , huntRankingCfg  :: RankConfig (Dt.DValue dt)
  , huntTypes       :: ContextTypes
  , huntNormalizers :: [CNormalizer]
  , huntQueryCfg    :: ProcessConfig
  }

type DefHuntEnv = HuntEnv (Documents CompressedDoc)

initHunt :: DocTable dt => IO (HuntEnv dt)
initHunt = initHuntEnv (ContextIx Ixx.empty Dt.empty M.empty) defaultRankConfig contextTypes normalizers queryConfig

contextTypes :: ContextTypes
contextTypes = [ctText, ctInt, ctDate, ctPosition]

normalizers :: [CNormalizer]
normalizers = [cnUpperCase, cnLowerCase, cnZeroFill]

queryConfig     :: ProcessConfig
queryConfig     = ProcessConfig (FuzzyConfig True True 1.0 germanReplacements) True 100 500

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

-- ----------------------------------------------------------------------------
-- the command evaluation monad
-- ----------------------------------------------------------------------------
newtype HuntT dt m a
  = HuntT { runHuntT :: ReaderT (HuntEnv dt) (ErrorT CmdError m) a }
  deriving (Applicative, Monad, MonadIO, Functor, MonadReader (HuntEnv dt), MonadError CmdError)

instance MonadTrans (HuntT dt) where
  lift = HuntT . lift . lift

type Hunt dt = HuntT dt IO

-- ----------------------------------------------------------------------------

runHunt :: DocTable dt => HuntT dt m a -> HuntEnv dt -> m (Either CmdError a)
runHunt env = runErrorT . runReaderT (runHuntT env)

runCmd :: (DocTable dt, Bin.Binary dt) => HuntEnv dt -> Command -> IO (Either CmdError CmdResult)
runCmd env cmd
  = runErrorT . runReaderT (runHuntT . execCommand $ cmd) $ env

askIx :: DocTable dt => Hunt dt (ContextIndex dt)
askIx = do
  ref <- asks huntIndex
  liftIO $ readXMVar ref

-- FIXME: io exception-safe?
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

withIx :: DocTable dt => (ContextIndex dt -> Hunt dt a) -> Hunt dt a
withIx f
  = askIx >>= f

askType :: DocTable dt => Text -> Hunt dt ContextType
askType cn = do
  ts <- asks huntTypes
  case L.find (\t -> cn == ctName t) ts of
    Just t -> return t
    _      -> throwResError 410 ("used unavailable context type: " `T.append` cn)

askNormalizer :: DocTable dt => Text -> Hunt dt CNormalizer
askNormalizer cn = do
  ts <- asks huntNormalizers
  case L.find (\t -> cn == cnName t) ts of
    Just t -> return t
    _      -> throwResError 410 ("used unavailable normalizer: " `T.append` cn)

askIndex :: DocTable dt => Text -> Hunt dt (Impl.IndexImpl Occurrences)
askIndex cn = askType cn >>= return . ctIxImpl

askContextsWeights :: DocTable dt => Hunt dt(M.Map Context CWeight)
askContextsWeights
  = withIx (\(ContextIx _ _ schema) -> return $ M.map cxWeight schema)

throwResError :: DocTable dt => Int -> Text -> Hunt dt a
throwResError n msg
    = do errorM $ unwords [show n, T.unpack msg]
         throwError $ ResError n msg

descending :: Ord a => a -> a -> Ordering
descending = flip compare

-- ----------------------------------------------------------------------------

execCommand :: (Bin.Binary dt, DocTable dt) => Command -> Hunt dt CmdResult
execCommand
  = execCmd . toBasicCommand

-- XXX: kind of obsolete now
execCmd :: (Bin.Binary dt, DocTable dt) => BasicCommand -> Hunt dt CmdResult
execCmd cmd@(InsertList _) = do
  debugM $ "Exec: InsertList [..]"
  execCmd' cmd
execCmd cmd = do
  debugM $ "Exec: " ++ logShow cmd
  execCmd' cmd


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

-- ----------------------------------------------------------------------------

execSequence :: (DocTable dt, Bin.Binary dt)=> [BasicCommand] -> Hunt dt CmdResult
execSequence []       = execCmd NOOP
execSequence [c]      = execCmd c
execSequence (c : cs) = execCmd c >> execSequence cs

execInsertContext :: DocTable dt
                  => Context
                  -> ContextSchema
                  -> ContextIndex dt
                  -> Hunt dt (ContextIndex dt, CmdResult)
execInsertContext cx ct ixx@(ContextIx ix dt s)
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
    return ( ContextIx { ixhIndex = Ixx.insertContext cx (newIx impl) ix
                 , ixhDocs   = dt
                 , ixhSchema = M.insert cx (ct
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
                  -> Hunt dt(ContextIndex dt, CmdResult)
execDeleteContext cx (ContextIx ix dt s)
  = return (ContextIx (Ixx.deleteContext cx ix) dt (M.delete cx s), ResOK)

-- | Inserts an 'ApiDocument' into the index.
-- /NOTE/: All contexts mentioned in the 'ApiDocument' need to exist.
-- Documents/URIs must not exist.
execInsertList :: DocTable dt
                => [ApiDocument] -> ContextIndex dt -> Hunt dt (ContextIndex dt, CmdResult)
execInsertList docs ixx@(ContextIx _ix _dt schema) = do
  -- TODO: use set for undup
  let contexts = concatMap (M.keys . apiDocIndexMap) docs
  checkContextsExistence contexts ixx
  -- apidoc should not exist
  mapM_ (flip (checkApiDocExistence False) ixx) docs
  let docsAndWords = map (toDocAndWords schema) docs
  ixx' <- lift $ Ixx.insertList docsAndWords ixx
  return (ixx', ResOK)


-- | Updates an 'ApiDocument'.
-- /NOTE/: All contexts mentioned in the 'ApiDocument' need to exist.
-- Documents/URIs need to exist.
execUpdate :: DocTable dt
           => ApiDocument -> ContextIndex dt -> Hunt dt(ContextIndex dt, CmdResult)
execUpdate doc ixx@(ContextIx _ix dt schema) = do
  let contexts = M.keys $ apiDocIndexMap doc
  checkContextsExistence contexts ixx
  let (docs, ws) = toDocAndWords schema doc
  docIdM <- lift $ Dt.lookupByURI (uri docs) dt
  case docIdM of
    Just docId -> do
      ixx' <- lift $ Ixx.modifyWithDescription (desc docs) ws docId ixx
      return (ixx', ResOK)
    Nothing    ->
      throwResError 409 $ "document for update not found: " `T.append` uri docs


unless' :: DocTable dt
       => Bool -> Int -> Text -> Hunt dt ()
unless' b code text = unless b $ throwResError code text


checkContextsExistence :: DocTable dt
                       => [Context] -> ContextIndex dt -> Hunt dt ()
checkContextsExistence cs ixx = do
  ixxContexts        <- S.fromList <$> Ixx.contexts ixx
  let docContexts     = S.fromList cs
  let invalidContexts = S.difference docContexts ixxContexts
  unless' (S.null invalidContexts)
    409 $ "mentioned context(s) are not present: "
            `T.append` (T.pack . show . S.toList) invalidContexts


checkApiDocExistence :: DocTable dt
                     => Bool -> ApiDocument -> ContextIndex dt -> Hunt dt ()
checkApiDocExistence switch apidoc ixx = do
  let u = apiDocUri apidoc
  mem <- Ixx.member u ixx
  unless' (switch == mem)
    409 $ (if mem
            then "document already exists: "
            else "document does not exist: ") `T.append` u


execSearch' :: (DocTable dt, e ~ Dt.DValue dt)
            => (Result e -> CmdResult)
            -> Query
            -> ContextIndex dt
            -> Hunt dt CmdResult
execSearch' f q (ContextIx ix dt s)
  = do
    cfg <- asks huntQueryCfg
    r   <- lift $ runQueryM ix s cfg dt q
    rc  <- asks huntRankingCfg
    cw  <- askContextsWeights
    case r of
      Left  err -> throwError err
      Right res -> return . f . rank rc cw $ res

-- FIXME: signature to result
wrapSearch :: (DocumentWrapper e) => Int -> Int -> Result e -> CmdResult
wrapSearch offset mx
  = ResSearch
    . mkLimitedResult offset mx
--    . map fst -- remove score from result
    . sortBy (descending `on` snd) -- sort by score
    . map (\(_did, (di, _dch)) -> (unwrap . document $ di, docScore di))
    . DM.toList
    . docHits

wrapCompletion :: Int -> Result e -> CmdResult
wrapCompletion mx
  = ResCompletion
    . take mx
    . map (\(word,_score,terms') -> (word, terms')) -- remove score from result
    . sortBy (descending `on` (\(_,score,_) -> score)) -- sort by score
    . map (\(c, (wi, _wch)) -> (c, wordScore wi, terms wi))
    . M.toList
    . wordHits


execDeleteDocs :: DocTable dt => Set URI -> ContextIndex dt -> Hunt dt(ContextIndex dt, CmdResult)
execDeleteDocs d ix = do
  ix' <- lift $ Ixx.deleteDocsByURI d ix
  return (ix', ResOK)


execDeleteByQuery :: DocTable dt => Query -> ContextIndex dt -> Hunt dt(ContextIndex dt, CmdResult)
execDeleteByQuery q ixx@(ContextIx ix _dt s) = do
  r <- lift $ runQueryDocIdsM ix s q
  case r of
    Left  err -> throwError err
    Right res -> do
      ix' <- lift $ Ixx.delete res ixx
      return (ix', ResOK)

-- ----------------------------------------------------------------------------

-- | general binary serialzation function
execStore :: (Bin.Binary a, DocTable dt) =>
             FilePath -> a -> Hunt dt CmdResult
execStore filename x = do
  liftIO $ Bin.encodeFile filename x
  return ResOK

-- | deserialization needs to be more specific because of our existential typed index
execLoad :: (Bin.Binary dt, DocTable dt) => FilePath -> Hunt dt (ContextIndex dt, CmdResult)
execLoad filename = do
  ts <- asks huntTypes
  let ix = map ctIxImpl ts
  ixh@(ContextIx _ _ s) <- liftIO $ decodeFile' ix filename
  ls <- TV.mapM reloadSchema s
  return (ixh{ ixhSchema = ls }, ResOK)
  where
  decodeFile' ts f = do
    bs <- BL.readFile f
    return $ decodeCxIx ts bs

  reloadSchema s = (askType . ctName . cxType) s >>= \t -> return $ s { cxType = t }

-- ----------------------------------------------------------------------------

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

-- | specific internal query config for delete query only
queryConfigDocIds :: ProcessConfig
queryConfigDocIds = ProcessConfig (FuzzyConfig True True 1.0 germanReplacements) True 0 0

runQueryDocIdsM :: Ixx.ContextMap Occurrences
                -> Schema
                -> Query
                -> IO (Either CmdError DocIdSet)
runQueryDocIdsM ix s q = processQueryDocIds st q
  where
  st = QProc.initProcessor queryConfigDocIds ix s

-- ----------------------------------------------------------------------------

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
        dumpDocTable (ContextIx _ix dt _s)
            = ResGeneric <$> Dt.toJSON'DocTable dt


execStatus StatusIndex
  = withIx
    (\_ix -> return $ ResGeneric $ JS.String "status of Index not yet implemented" )

-- ----------------------------------------------------------------------------
