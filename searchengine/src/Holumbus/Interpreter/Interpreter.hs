{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}


module Holumbus.Interpreter.Interpreter where

import           Control.Applicative
import           Control.Concurrent.XMVar
import           Control.Monad.Error
import           Control.Monad.Reader

import qualified Data.Aeson                                  as JS
import qualified Data.Binary                                 as Bin
import qualified Data.ByteString.Lazy                        as BL
import           Data.Function                               (on)
import           Data.List                                   (groupBy, sortBy)
import qualified Data.List                                   as L
import qualified Data.Map                                    as M
import           Data.Maybe                                  (fromMaybe)
import           Data.Set                                    (Set)
import qualified Data.Set                                    as S
import           Data.Text                                   (Text)
import qualified Data.Text                                   as T
import qualified Data.Traversable                            as TV

import           Holumbus.Common
import           Holumbus.Common.ApiDocument                 as ApiDoc
import qualified Holumbus.Common.DocIdMap                    as DM
import           Holumbus.Common.Document                    (DocumentWrapper,
                                                              unwrap)
import           Holumbus.Common.Document.Compression.BZip   (CompressedDoc)
--import           Holumbus.Common.Document.Compression.Snappy (CompressedDoc)

import qualified Holumbus.Index.Index                        as Ix
import           Holumbus.Index.Schema.Analyze

import           Holumbus.IndexHandler                       (IndexHandler (..),
                                                              decodeIXH)
import qualified Holumbus.IndexHandler                       as Ixx

import           Holumbus.Index.IndexImpl                    (IndexImpl (..),
                                                              mkIndex)
import qualified Holumbus.Index.IndexImpl                    as Impl
import           Holumbus.Index.Proxy.ContextIndex           (ContextIndex)
import qualified Holumbus.Index.Proxy.ContextIndex           as CIx

import           Holumbus.Query.Fuzzy
import           Holumbus.Query.Language.Grammar
--import           Holumbus.Query.Language.Parser
import           Holumbus.Query.Processor
import           Holumbus.Query.Ranking
import           Holumbus.Query.Result                       as QRes

import           Holumbus.DocTable.DocTable                  (DocTable)
import qualified Holumbus.DocTable.DocTable                  as Dt
import           Holumbus.DocTable.HashedDocTable            as HDt

import           Holumbus.Interpreter.Command

import qualified System.Log.Logger                           as Log

import           Holumbus.Utility.Log

import           GHC.Stats
import           GHC.Stats.Json                              ()

-- ----------------------------------------------------------------------------
--
-- the semantic domains (datatypes for interpretation)
--
-- Env, Index, ...

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
modName = "Holumbus.Interpreter.Interpreter"

-- | Log a message at 'DEBUG' priority.
debugM :: String -> IO ()
debugM = Log.debugM modName

-- | Log a message at 'WARNING' priority.
warningM :: String -> IO ()
warningM = Log.warningM modName

-- | Log a message at 'ERROR' priority.
errorM :: String -> IO ()
errorM = Log.errorM modName

-- | Log formated values that get inserted into a context
debugContext :: Context -> Words -> IO ()
debugContext c ws = debugM $ concat ["insert in", T.unpack c, show . M.toList $ fromMaybe M.empty $ M.lookup c ws]

-- ----------------------------------------------------------------------------

emptyIndexer :: IndexHandler (Documents CompressedDoc)
emptyIndexer = IXH CIx.empty HDt.empty M.empty

-- ----------------------------------------------------------------------------

{--contextTypes :: ContextTypes
contextTypes
  = M.fromList $
      [ ("text",     Impl.CxMeta CText     defaultInv)
      , ("int",      Impl.CxMeta CInt      intInv)
      , ("date",     Impl.CxMeta CDate     dateInv)
      , ("position", Impl.CxMeta CPosition positionInv)
      ]
--}

contextTypes :: ContextTypes
contextTypes = [ctText, ctInt, ctDate, ctPosition]


-- ----------------------------------------------------------------------------
--
-- the environment
-- with a MVar for storing the index
-- so the MVar acts as a global state (within IO)
data Env dt = Env
  { evIndexer :: DocTable dt => XMVar (IndexHandler dt)
  , evRanking :: RankConfig (Dt.DValue dt)
  , evCxTypes :: ContextTypes
  }

initEnv :: DocTable dt => IndexHandler dt -> RankConfig (Dt.DValue dt) -> ContextTypes -> IO (Env dt)
initEnv ixx rnk opt = do
  ixref <- newXMVar ixx
  return $ Env ixref rnk opt

-- ----------------------------------------------------------------------------
-- the command evaluation monad
-- ----------------------------------------------------------------------------
newtype CMT dt m a = CMT { runCMT :: ReaderT (Env dt) (ErrorT CmdError m) a }
  deriving (Applicative, Monad, MonadIO, Functor, MonadReader (Env dt), MonadError CmdError)

instance MonadTrans (CMT dt) where
  lift = CMT . lift . lift

type CM dt = CMT dt IO

-- ----------------------------------------------------------------------------

runCM :: DocTable dt => CMT dt m a -> Env dt -> m (Either CmdError a)
runCM env = runErrorT . runReaderT (runCMT env)

runCmd :: (DocTable dt, Bin.Binary dt) => Env dt -> Command -> IO (Either CmdError CmdResult)
runCmd env cmd
    = runErrorT . runReaderT (runCMT . execCmd $ cmd) $ env

askIx :: DocTable dt => CM dt (IndexHandler dt)
askIx
    = do ref <- asks evIndexer
         liftIO $ readXMVar ref

-- FIXME: io exception-safe?
modIx :: DocTable dt
      => (IndexHandler dt-> CM dt (IndexHandler dt, a)) -> CM dt a
modIx f
    = do ref <- asks evIndexer
         ix <- liftIO $ takeXMVarWrite ref
         (i',a) <- f ix `catchError` putBack ref ix
         liftIO $ putXMVarWrite ref i'
         return a
    where
    putBack ref i e = do
        liftIO $ putXMVarWrite ref i
        throwError e

modIx_ :: DocTable dt => (IndexHandler dt -> CM dt (IndexHandler dt)) -> CM dt()
modIx_ f = modIx f'
    where f' i = f i >>= \r -> return (r, ())

withIx :: DocTable dt => (IndexHandler dt -> CM dt a) -> CM dt a
withIx f
    = askIx >>= f

askTypes :: DocTable dt => CM dt ContextTypes
askTypes
    = asks evCxTypes

askType :: DocTable dt => Text -> CM dt ContextType
askType cn = do
    ts <- askTypes
    case L.find (\t -> cn == ctName t) ts of
      (Just t) -> return t
      _        -> throwResError 410 ("used unavailable context type: " `T.append` cn)

askIndex :: DocTable dt => Text -> CM dt (Impl.IndexImpl Occurrences)
askIndex cn = do
    t <- askType cn
    return $ ctIxImpl t

askRanking :: DocTable dt => CM dt (RankConfig (Dt.DValue dt))
askRanking
    = asks evRanking

-- TODO: meh
askContextsWeights :: DocTable dt => CM dt(M.Map Context CWeight)
askContextsWeights
    = withIx (\(IXH _ _ schema) -> return $ M.map cxWeight schema)

throwResError :: DocTable dt => Int -> Text -> CM dt a
throwResError n msg
    = throwError $ ResError n msg

throwNYI :: DocTable dt => String -> CM dt a
throwNYI c = throwResError 501 $ "command not yet implemented: " `T.append` T.pack c

descending :: Ord a => a -> a -> Ordering
descending = flip compare

-- ----------------------------------------------------------------------------

-- optimize a command/command sequence
-- delete and batchDelete are both part of the Command datatype, but only BatchDelete should be
-- present for execution
-- an intermediary type may be necessary to ensure that on type-level, e.g.
--   optimizeCmd :: Command -> ExecCommand  &&  execCmd :: ExecCommand -> CM CmdResult

optimizeCmd :: Command -> Command
optimizeCmd (Sequence cs) = Sequence $ opt cs
  where
  opt :: [Command] -> [Command]
  opt cs' = concatMap optGroup $ groupBy equalHeads cs'
  -- requires the commands to be grouped by constructor
  optGroup :: [Command] -> [Command]
  -- groups of delete to BatchDelete
  optGroup cs'@(Delete{}:_)
    = foldl (\(BatchDelete us) (Delete u) -> BatchDelete (S.insert u us)) (BatchDelete S.empty) cs' : []
  -- optimize nested sequences too
  -- XXX: maybe flatten sequences
  optGroup cs'@(Sequence{}:_)
    = map optimizeCmd cs'
  optGroup cs' = cs'
  -- group by constructor
  -- NOTE: just delete and sequence because that are the only optimizations for now
  equalHeads :: Command -> Command -> Bool
  equalHeads Delete{}   Delete{}   = True
  equalHeads Sequence{} Sequence{} = True
  equalHeads _ _                   = False

-- a single Delete is not allowed
optimizeCmd (Delete u) = BatchDelete $ S.singleton u
optimizeCmd c = c


execCmd :: (Bin.Binary dt, DocTable dt) => Command -> CM dt CmdResult
execCmd cmd = do
  liftIO $ debugM $ "Exec: " ++ logShow cmd
  execCmd' . optimizeCmd $ cmd

execCmd' :: (Bin.Binary dt, DocTable dt) => Command -> CM dt CmdResult
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

execCmd' (Insert doc)
    = modIx $ execInsert doc

execCmd' (Update doc)
    = modIx $ execUpdate doc

execCmd' (Delete _uri)
    = error "execCmd' (Delete{})" --modIx $ execDelete uri

execCmd' (BatchDelete uris)
    = modIx $ execBatchDelete uris

execCmd' (StoreIx filename)
    = withIx $ execStore filename

execCmd' (LoadIx filename)
    = modIx $ \_ix -> execLoad filename

execCmd' (InsertContext cx ct)
    = modIx $ execInsertContext cx ct

execCmd' (DeleteContext cx)
    = modIx $ execDeleteContext cx

-- ----------------------------------------------------------------------------

execSequence :: (DocTable dt, Bin.Binary dt)=> [Command] -> CM dt CmdResult
execSequence []       = execCmd NOOP
execSequence [c]      = execCmd c
execSequence (c : cs) = execCmd c >> execSequence cs

execInsertContext :: DocTable dt
                  => Context
                  -> ContextSchema
                  -> IndexHandler dt
                  -> CM dt (IndexHandler dt, CmdResult)
execInsertContext cx ct ixx@(IXH ix dt s)
    = do
      -- check if context already exists
      contextExists        <- Ixx.hasContext cx ixx
      unless' (not contextExists)
             409 $ "context already exists: " `T.append` cx

      -- check if type exists in this interpreter instance
      cType                <- askType . cxName $ ct
      impl                 <- askIndex . cxName $ ct

      -- create new index instance and insert it with context
      return ( IXH { ixhIndex = CIx.insertContext cx (newIx impl) ix
                   , ixhDocs   = dt
                   , ixhSchema = M.insert cx (ct {cxType = cType}) s
                   }
             , ResOK )
      where
      newIx (IndexImpl i) = mkIndex $ Ix.empty `asTypeOf` i

-- | Deletes the context and the schema associated with it.
execDeleteContext :: DocTable dt
                  => Context
                  -> IndexHandler dt
                  -> CM dt(IndexHandler dt, CmdResult)
execDeleteContext cx (IXH ix dt s)
  = return (IXH (CIx.deleteContext cx ix) dt (M.delete cx s), ResOK)


-- | Inserts an 'ApiDocument' into the index.
-- /NOTE/: All contexts mentioned in the 'ApiDocument' need to exist.
-- Documents/URIs must not exist.
execInsert :: DocTable dt
           => ApiDocument -> IndexHandler dt -> CM dt (IndexHandler dt, CmdResult)
execInsert doc ixx@(IXH _ix _dt schema) = do
    let contexts = M.keys $ apiDocIndexMap doc
    checkContextsExistence contexts ixx
    -- apidoc should not exist
    checkApiDocExistence False doc ixx
    let (docs, ws) = toDocAndWords schema doc

    liftIO $ debugContext "contextgeo" ws
    liftIO $ debugContext "contextint" ws
    liftIO $ debugContext "contextdate" ws

    ixx' <- lift $ Ixx.insert docs ws ixx
    return (ixx', ResOK)


-- | Updates an 'ApiDocument'.
-- /NOTE/: All contexts mentioned in the 'ApiDocument' need to exist.
-- Documents/URIs need to exist.
execUpdate :: DocTable dt
           => ApiDocument -> IndexHandler dt -> CM dt(IndexHandler dt, CmdResult)
execUpdate doc ixx@(IXH _ix dt schema) = do
    let contexts = M.keys $ apiDocIndexMap doc
    checkContextsExistence contexts ixx
    let (docs, ws) = toDocAndWords schema doc
    docIdM <- lift $ Dt.lookupByURI dt (uri docs)
    case docIdM of
      Just docId -> do
        ixx' <- lift $ Ixx.modifyWithDescription (desc docs) ws docId ixx
        return (ixx', ResOK)
      Nothing    ->
        throwResError 409 $ "document for update not found: " `T.append` uri docs


unless' :: DocTable dt
       => Bool -> Int -> Text -> CM dt()
unless' b code text = unless b $ throwResError code text


checkContextsExistence :: DocTable dt
                       => [Context] -> IndexHandler dt -> CM dt()
checkContextsExistence cs ixx = do
  ixxContexts        <- S.fromList <$> Ixx.contexts ixx
  let docContexts     = S.fromList cs
  let invalidContexts = S.difference docContexts ixxContexts
  unless' (S.null invalidContexts)
    409 $ "mentioned context(s) are not present: "
            `T.append` (T.pack . show . S.toList) invalidContexts


checkApiDocExistence :: DocTable dt
                     => Bool -> ApiDocument -> IndexHandler dt -> CM dt()
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
            -> IndexHandler dt
            -> CM dt CmdResult
execSearch' f q (IXH ix dt s)
    = do
      r <- lift $ runQueryM ix s dt q
      rc <- askRanking
      cw <- askContextsWeights
      case r of
        (Left  err) -> throwError err
        (Right res) -> return . f . rank rc cw $ res

-- FIXME: signature to result
wrapSearch :: (DocumentWrapper e) => Int -> Int -> Result e -> CmdResult
wrapSearch offset mx
    = ResSearch
      . mkLimitedResult offset mx
      . map fst -- remove score from result
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


execBatchDelete :: DocTable dt => Set URI -> IndexHandler dt -> CM dt(IndexHandler dt, CmdResult)
execBatchDelete d ix = do
    ix' <- lift $ Ixx.deleteDocsByURI d ix
    return (ix', ResOK)

-- ----------------------------------------------------------------------------

-- | general binary serialzation function
execStore :: (Bin.Binary a, DocTable dt) =>
             FilePath -> a -> CM dt CmdResult
execStore filename x = do
    liftIO $ Bin.encodeFile filename x
    return ResOK

-- | deserialization needs to be more specific because of our existential typed index
execLoad :: (Bin.Binary dt, DocTable dt) => FilePath -> CM dt (IndexHandler dt, CmdResult)
execLoad filename = do
    ts <- askTypes
    let ix = map ctIxImpl ts
    ixh@(IXH _ _ s) <- liftIO $ decodeFile' ix filename
    ls <- TV.mapM reloadSchema s
    return (ixh{ ixhSchema = ls }, ResOK)
    where
    decodeFile' ts f = do
       bs <- BL.readFile f
       return $ decodeIXH ts bs

    reloadSchema s = (askType . ctName . cxType) s >>= \t -> return $ s { cxType = t }

-- ----------------------------------------------------------------------------

queryConfig     :: ProcessConfig
queryConfig     = ProcessConfig (FuzzyConfig True True 1.0 germanReplacements) True 100 500

runQueryM       :: DocTable dt
                => ContextIndex Occurrences
                -> Schema
                -> dt
                -> Query
                -> IO (Either CmdError (QRes.Result (Dt.DValue dt)))
runQueryM ix s dt q = processQuery st dt q
   where
   st = initState queryConfig ix s

-- ----------------------------------------------------------------------------

execStatus :: DocTable dt => StatusCmd -> CM dt CmdResult
execStatus StatusGC
    = do statsEnabled <- liftIO getGCStatsEnabled
         if statsEnabled
           then liftIO getGCStats >>= return . ResGeneric . JS.toJSON
           else throwResError 501 ("GC stats not enabled. Use `+RTS -T -RTS' to enable them." :: Text)

execStatus StatusIndex
    = withIx
      ( \ _ix -> return $ ResGeneric $ JS.String "status of Index not yet implemented" )

-- ----------------------------------------------------------------------------
