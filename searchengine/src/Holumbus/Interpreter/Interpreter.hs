{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Holumbus.Interpreter.Interpreter where

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Reader

import qualified Data.Binary                                 as Bin
import           Data.Function                               (on)
import           Data.List                                   (groupBy, sortBy)
import qualified Data.Map                                    as M
import           Data.Maybe                                  (fromMaybe)
import           Data.Set                                    (Set)
import qualified Data.Set                                    as S
import           Data.Text                                   (Text)
import qualified Data.Text                                   as T

import           Holumbus.Common
import           Holumbus.Common.ApiDocument                 as ApiDoc
import qualified Holumbus.Common.DocIdMap                    as DM
import           Holumbus.Common.Document                    (DocumentWrapper,
                                                              unwrap)
import           Holumbus.Common.Document.Compression.BZip   (CompressedDoc)

import           Holumbus.Index.Schema.Analyze

import           Holumbus.Indexer.TextIndexer                (ContextTextIndexer,
                                                              TextIndexerCon)
import qualified Holumbus.Indexer.TextIndexer                as Ixx

import           Holumbus.Index.InvertedIndex
import           Holumbus.Index.Proxy.ContextIndex           (ContextIndex)
import qualified Holumbus.Index.Proxy.ContextIndex           as CIx
import qualified Holumbus.Index.Index                        as Ix

import           Holumbus.Query.Fuzzy
import           Holumbus.Query.Language.Grammar
--import           Holumbus.Query.Language.Parser
import           Holumbus.Query.Processor
import           Holumbus.Query.Ranking
import           Holumbus.Query.Result                       as QRes

import qualified Holumbus.DocTable.DocTable                  as Dt
import           Holumbus.DocTable.HashedDocTable            as HDt

import           Holumbus.Interpreter.Command

import qualified System.Log.Logger                           as Log

import           Holumbus.Utility.Log
import           Holumbus.Utility.XMVar

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

type IpIndexer dt = ContextTextIndexer dt

emptyIndexer :: IpIndexer (Documents CompressedDoc)
emptyIndexer = (CIx.empty, HDt.empty, M.empty)

-- ----------------------------------------------------------------------------

type ContextTypes = M.Map CType (Ix.IndexImpl Occurrences)

contextTypes :: ContextTypes
contextTypes  = M.fromList $
                [ (CText,     Ix.IndexImpl $ (Ix.empty :: InvertedIndex Occurrences))
                , (CInt,      Ix.IndexImpl $ (Ix.empty :: InvertedIndex Occurrences))
                , (CDate,     Ix.IndexImpl $ (Ix.empty :: InvertedIndex Occurrences))
                , (CPosition, Ix.IndexImpl $ (Ix.empty :: InvertedIndex Occurrences)) 
                ]

-- ----------------------------------------------------------------------------
--
-- the environment
-- with a MVar for storing the index
-- so the MVar acts as a global state (within IO)

data Env dt = Env
    { evIndexer :: TextIndexerCon dt => XMVar (IpIndexer dt)
    , evRanking :: RankConfig (Dt.DValue dt)
    , evCxTypes :: ContextTypes
    }

initEnv :: TextIndexerCon dt => IpIndexer dt -> RankConfig (Dt.DValue dt) -> ContextTypes -> IO (Env dt)
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

runCM :: TextIndexerCon dt => CMT dt m a -> Env dt -> m (Either CmdError a)
runCM env = runErrorT . runReaderT (runCMT env)

runCmd :: TextIndexerCon dt => Env dt -> Command -> IO (Either CmdError CmdResult)
runCmd env cmd
    = runErrorT . runReaderT (runCMT . execCmd $ cmd) $ env

askIx :: TextIndexerCon dt => CM dt (IpIndexer dt)
askIx
    = do ref <- asks evIndexer
         liftIO $ readXMVar ref

-- FIXME: io exception-safe?
modIx :: TextIndexerCon dt
      => (IpIndexer dt-> CM dt (IpIndexer dt, a)) -> CM dt a
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

modIx_ :: TextIndexerCon dt => (IpIndexer dt -> CM dt (IpIndexer dt)) -> CM dt()
modIx_ f = modIx f'
    where f' i = f i >>= \r -> return (r, ())

withIx :: TextIndexerCon dt => (IpIndexer dt -> CM dt a) -> CM dt a
withIx f
    = askIx >>= f

askTypes :: TextIndexerCon dt => CM dt ContextTypes
askTypes
    = asks evCxTypes

askRanking :: TextIndexerCon dt => CM dt (RankConfig (Dt.DValue dt))
askRanking
    = asks evRanking

-- TODO: meh
askContextsWeights :: TextIndexerCon dt => CM dt(M.Map Context CWeight)
askContextsWeights
    = withIx (\(_,_,schema) -> return $ M.map cxWeight schema)

throwResError :: TextIndexerCon dt => Int -> Text -> CM dt a
throwResError n msg
    = throwError $ ResError n msg

throwNYI :: TextIndexerCon dt => String -> CM dt a
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


execCmd :: (Bin.Binary dt) => TextIndexerCon dt => Command -> CM dt CmdResult
execCmd cmd = do
  liftIO $ debugM $ "Executing command: " ++ logShow cmd
  execCmd' . optimizeCmd $ cmd

execCmd' :: (Bin.Binary dt, TextIndexerCon dt) => Command -> CM dt CmdResult
execCmd' (Search q offset mx)
    = withIx $ execSearch' (wrapSearch offset mx) q

execCmd' (Completion q mx)
    = withIx $ execSearch' (wrapCompletion mx) q

execCmd' (Sequence cs)
    = execSequence cs

execCmd' NOOP
    = return ResOK  -- keep alive test

execCmd' (Insert doc)
    = modIx $ execInsert doc

execCmd' (Update doc)
    = modIx $ execUpdate doc

execCmd' (Delete _uri)
    = error "execCmd' (Delete{})" --modIx $ execDelete uri

execCmd' (BatchDelete uris)
    = modIx $ execBatchDelete uris

execCmd' (StoreIx filename)
    = undefined -- withIx $ execStore filename

execCmd' (LoadIx filename)
    = undefined -- modIx $ \_ix -> execLoad filename

execCmd' (InsertContext cx ct)
    = modIx $ execInsertContext cx ct

execCmd' (DeleteContext cx)
    = modIx $ execDeleteContext cx

-- ----------------------------------------------------------------------------

execSequence :: TextIndexerCon dt => [Command] -> CM dt CmdResult
execSequence []       = execCmd NOOP
execSequence [c]      = execCmd c
execSequence (c : cs) = execCmd c >> execSequence cs


execInsertContext :: TextIndexerCon dt
                  => Context
                  -> ContextSchema
                  -> IpIndexer dt
                  -> CM dt (IpIndexer dt, CmdResult)
execInsertContext cx ct ixx@(ix, dt, s)
    = do
      contextExists        <- Ixx.hasContext cx ixx
      unless' (not contextExists)
             409 $ "context already exists: " `T.append` cx
      return (ixx', ResOK)
    where
    ixx' = ( CIx.insertContext cx newIx ix
           , dt
           , M.insert cx ct s)

    -- create contexttype dependend index here for insertion
    newIx = Ix.empty :: InvertedIndex Occurrences

-- | Deletes the context and the schema associated with it.
execDeleteContext :: TextIndexerCon dt
                  => Context
                  -> IpIndexer dt
                  -> CM dt(IpIndexer dt, CmdResult)
execDeleteContext cx (ix, dt, s)
  = return ((CIx.deleteContext cx ix, dt, M.delete cx s), ResOK)


-- | Inserts an 'ApiDocument' into the index.
-- /NOTE/: All contexts mentioned in the 'ApiDocument' need to exist.
-- Documents/URIs must not exist.
execInsert :: TextIndexerCon dt
           => ApiDocument -> IpIndexer dt -> CM dt(IpIndexer dt, CmdResult)
execInsert doc ixx@(_ix, _dt, schema) = do
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
execUpdate :: TextIndexerCon dt
           => ApiDocument -> IpIndexer dt -> CM dt(IpIndexer dt, CmdResult)
execUpdate doc ixx@(_ix, dt, schema) = do
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


unless' :: TextIndexerCon dt
       => Bool -> Int -> Text -> CM dt()
unless' b code text = unless b $ throwResError code text


checkContextsExistence :: TextIndexerCon dt
                       => [Context] -> IpIndexer dt -> CM dt()
checkContextsExistence cs ixx = do
  ixxContexts        <- S.fromList <$> Ixx.contexts ixx
  let docContexts     = S.fromList cs
  let invalidContexts = S.difference docContexts ixxContexts
  unless' (S.null invalidContexts)
    409 $ "mentioned context(s) are not present: "
            `T.append` (T.pack . show . S.toList) invalidContexts


checkApiDocExistence :: TextIndexerCon dt
                     => Bool -> ApiDocument -> IpIndexer dt -> CM dt()
checkApiDocExistence switch apidoc ixx = do
  let u = apiDocUri apidoc
  mem <- Ixx.member u ixx
  unless' (switch == mem)
    409 $ (if mem
            then "document already exists: "
            else "document does not exist: ") `T.append` u


execSearch' :: (TextIndexerCon dt, e ~ Dt.DValue dt)
            => (Result e -> CmdResult)
            -> Query
            -> IpIndexer dt
            -> CM dt CmdResult
execSearch' f q (ix, dt, s)
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
      . map fst -- remove score from result
      . sortBy (descending `on` snd) -- sort by score
      . map (\(c, (wi, _wch)) -> (c, wordScore wi))
      . M.toList
      . wordHits


execBatchDelete :: TextIndexerCon dt => Set URI -> IpIndexer dt -> CM dt(IpIndexer dt, CmdResult)
execBatchDelete d ix = do
    ix' <- lift $ Ixx.deleteDocsByURI d ix
    return (ix', ResOK)


execStore :: (Bin.Binary a, TextIndexerCon dt) =>
             FilePath -> a -> CM dt CmdResult
execStore filename x = do
    liftIO $ Bin.encodeFile filename x
    return ResOK


execLoad :: (Bin.Binary a, TextIndexerCon dt) =>
             FilePath -> CM dt (a, CmdResult)
execLoad filename = do
    x <- liftIO $ Bin.decodeFile filename
    return (x, ResOK)

-- ----------------------------------------------------------------------------

queryConfig     :: ProcessConfig
queryConfig     = ProcessConfig (FuzzyConfig True True 1.0 germanReplacements) True 100 500

runQueryM       :: TextIndexerCon dt
                => ContextIndex Occurrences
                -> Schema
                -> dt
                -> Query
                -> IO (Either CmdError (QRes.Result (Dt.DValue dt)))
runQueryM ix s dt q = processQuery st dt q
   where
   st = initState queryConfig ix s 2
