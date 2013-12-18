{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Holumbus.Interpreter.Interpreter where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad.Error
import           Control.Monad.Reader

import qualified Data.Binary                       as Bin
import           Data.Function                     (on)
import           Data.List                         (groupBy, sortBy)
import qualified Data.Map                          as M
import           Data.Set                          (Set)
import qualified Data.Set                          as S
import           Data.Text                         (Text)
import qualified Data.Text                         as T

import           Holumbus.Common
import           Holumbus.Common.ApiDocument       as ApiDoc
import qualified Holumbus.Common.DocIdMap          as DM

import           Holumbus.Index.Schema.Analyze

import           Holumbus.Indexer.TextIndexer      (ContextTextIndexer,
                                                    TextIndexerCon)
import qualified Holumbus.Indexer.TextIndexer      as Ixx

import           Holumbus.Index.InvertedIndex
import           Holumbus.Index.Proxy.ContextIndex (ContextIndex)
import qualified Holumbus.Index.Proxy.ContextIndex as CIx

import           Holumbus.Query.Fuzzy
import           Holumbus.Query.Language.Grammar
--import           Holumbus.Query.Language.Parser
import           Holumbus.Query.Processor
import           Holumbus.Query.Ranking
import           Holumbus.Query.Result             as QRes

import qualified Holumbus.DocTable.DocTable       as Dt
import           Holumbus.DocTable.HashedDocTable as HDt

import           Holumbus.Interpreter.Command

import qualified System.Log.Logger                as Log
import           Holumbus.Utility.Log

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

-- ----------------------------------------------------------------------------

type IpIndexer ix dt = ContextTextIndexer ix dt

emptyIndexer :: IpIndexer InvertedIndex (Documents Document)
emptyIndexer = (CIx.empty, HDt.empty, M.empty)

-- ----------------------------------------------------------------------------

data Options = Options

emptyOptions :: Options
emptyOptions = Options

-- ----------------------------------------------------------------------------
--
-- the environment
-- with a MVar for storing the index
-- so the MVar acts as a global state (within IO)

data Env ix dt = Env
    { evIndexer :: TextIndexerCon ix dt => MVar (IpIndexer ix dt)
    , evRanking :: RankConfig Document
    , evOptions :: Options
    }

initEnv :: TextIndexerCon ix dt => IpIndexer ix dt -> RankConfig Document -> Options -> IO (Env ix dt)
initEnv ixx rnk opt = do
  ixref <- newMVar ixx
  return $ Env ixref rnk opt

-- ----------------------------------------------------------------------------
-- the command evaluation monad
-- ----------------------------------------------------------------------------
newtype CMT ix dt m a = CMT { runCMT :: ReaderT (Env ix dt) (ErrorT CmdError m) a }
  deriving (Applicative, Monad, MonadIO, Functor, MonadReader (Env ix dt), MonadError CmdError)

instance MonadTrans (CMT ix dt) where
  lift = CMT . lift . lift

type CM ix dt = CMT ix dt IO

-- ----------------------------------------------------------------------------

runCM :: TextIndexerCon ix dt => CMT ix dt m a -> Env ix dt -> m (Either CmdError a)
runCM env = runErrorT . runReaderT (runCMT env)

runCmd :: TextIndexerCon ix dt => Env ix dt -> Command -> IO (Either CmdError CmdResult)
runCmd env cmd
    = runErrorT . runReaderT (runCMT . execCmd $ cmd) $ env

askIx :: TextIndexerCon ix dt => CM ix dt (IpIndexer ix dt)
askIx
    = do ref <- asks evIndexer
         liftIO $ readMVar ref

-- FIXME: io exception-safe?
modIx :: TextIndexerCon ix dt
      => (IpIndexer ix dt-> CM ix dt (IpIndexer ix dt, a)) -> CM ix dt a
modIx f
    = do ref <- asks evIndexer
         ix <- liftIO $ takeMVar ref
         (i',a) <- f ix `catchError` putBack ref ix
         liftIO $ putMVar ref i'
         return a
    where
    putBack ref i e = do
        liftIO $ putMVar ref i
        throwError e

modIx_ :: TextIndexerCon ix dt => (IpIndexer ix dt -> CM ix dt (IpIndexer ix dt)) -> CM ix dt ()
modIx_ f = modIx f'
    where f' i = f i >>= \r -> return (r, ())

withIx :: TextIndexerCon ix dt => (IpIndexer ix dt -> CM ix dt a) -> CM ix dt a
withIx f
    = askIx >>= f

askOpts :: TextIndexerCon ix dt => CM ix dt Options
askOpts
    = asks evOptions

askRanking :: TextIndexerCon ix dt => CM ix dt (RankConfig Document)
askRanking
    = asks evRanking

-- TODO: meh
askContextsWeights :: TextIndexerCon ix dt => CM ix dt (M.Map Context CWeight)
askContextsWeights
    = withIx (\(_,_,schema) -> return $ M.map cxWeight schema)

throwResError :: TextIndexerCon ix dt => Int -> Text -> CM ix dt a
throwResError n msg
    = throwError $ ResError n msg

throwNYI :: TextIndexerCon ix dt => String -> CM ix dt a
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


execCmd :: (Bin.Binary dt) => TextIndexerCon ix dt => Command -> CM ix dt CmdResult
execCmd cmd = do
  liftIO $ debugM $ "Executing command: " ++ logShow cmd
  execCmd' . optimizeCmd $ cmd

execCmd' :: (Bin.Binary dt, TextIndexerCon ix dt) => Command -> CM ix dt CmdResult
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
    = withIx $ execStore filename

execCmd' (LoadIx filename)
    = modIx $ \_ix -> execLoad filename

execCmd' (InsertContext cx ct)
    = modIx $ execInsertContext cx ct

execCmd' (DeleteContext cx)
    = modIx $ execDeleteContext cx

-- ----------------------------------------------------------------------------

execSequence :: TextIndexerCon ix dt => [Command] -> CM ix dt CmdResult
execSequence []       = execCmd NOOP
execSequence [c]      = execCmd c
execSequence (c : cs) = execCmd c >> execSequence cs


execInsertContext :: TextIndexerCon ix dt
                  => Context
                  -> ContextSchema
                  -> IpIndexer ix dt
                  -> CM ix dt (IpIndexer ix dt, CmdResult)
execInsertContext cx ct ixx@(ix, dt, s)
    = do
      contextExists        <- Ixx.hasContext cx ixx
      unless' (not contextExists)
             409 $ "context already exists: " `T.append` cx
      return (ixx', ResOK)
    where
    ixx' = ( CIx.insertContext cx ix
           , dt
           , M.insert cx ct s)

-- | Deletes the context and the schema associated with it.
execDeleteContext :: TextIndexerCon ix dt
                  => Context
                  -> IpIndexer ix dt
                  -> CM ix dt (IpIndexer ix dt, CmdResult)
execDeleteContext cx (ix, dt, s)
  = return ((CIx.deleteContext cx ix, dt, M.delete cx s), ResOK)


-- | Inserts an 'ApiDocument' into the index.
-- /NOTE/: All contexts mentioned in the 'ApiDocument' need to exist.
-- Documents/URIs must not exist.
execInsert :: TextIndexerCon ix dt
           => ApiDocument -> IpIndexer ix dt -> CM ix dt (IpIndexer ix dt, CmdResult)
execInsert doc ixx@(_ix, _dt, schema) = do
    let contexts = M.keys $ apiDocIndexMap doc
    checkContextsExistence contexts ixx
    -- apidoc should not exist
    checkApiDocExistence False doc ixx

    let (docs, ws) = toDocAndWords schema doc
    ixx' <- lift $ Ixx.insert docs ws ixx
    return (ixx', ResOK)


-- | Updates an 'ApiDocument'.
-- /NOTE/: All contexts mentioned in the 'ApiDocument' need to exist.
-- Documents/URIs need to exist.
execUpdate :: TextIndexerCon ix dt
           => ApiDocument -> IpIndexer ix dt -> CM ix dt (IpIndexer ix dt, CmdResult)
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


unless' :: TextIndexerCon ix dt
       => Bool -> Int -> Text -> CM ix dt ()
unless' b code text = unless b $ throwResError code text


checkContextsExistence :: TextIndexerCon ix dt
                       => [Context] -> IpIndexer ix dt -> CM ix dt ()
checkContextsExistence cs ixx = do
  ixxContexts        <- S.fromList <$> Ixx.contexts ixx
  let docContexts     = S.fromList cs
  let invalidContexts = S.difference docContexts ixxContexts
  unless' (S.null invalidContexts)
    409 $ "mentioned context(s) are not present: "
            `T.append` (T.pack . show . S.toList) invalidContexts


checkApiDocExistence :: TextIndexerCon ix dt
                     => Bool -> ApiDocument -> IpIndexer ix dt -> CM ix dt ()
checkApiDocExistence switch apidoc ixx = do
  let u = apiDocUri apidoc
  mem <- Ixx.member u ixx
  unless' (switch == mem)
    409 $ (if mem
            then "document already exists: "
            else "document does not exist: ") `T.append` u


execSearch' :: TextIndexerCon ix dt
            => (Result Document -> CmdResult)
            -> Query
            -> IpIndexer ix dt
            -> CM ix dt CmdResult
execSearch' f q (ix, dt, s)
    = do
      r <- lift $ runQueryM ix s dt q
      rc <- askRanking
      cw <- askContextsWeights
      case r of
        (Left  err) -> throwError err
        (Right res) -> return . f . rank rc cw $ res

wrapSearch :: Int -> Int -> Result Document -> CmdResult
wrapSearch offset mx
    = ResSearch
      . mkLimitedResult offset mx
      . map fst -- remove score from result
      . sortBy (descending `on` snd) -- sort by score
      . map (\(_did, (di, _dch)) -> (document di, docScore di))
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


execBatchDelete :: TextIndexerCon ix dt => Set URI -> IpIndexer ix dt -> CM ix dt (IpIndexer ix dt, CmdResult)
execBatchDelete d ix = do
    ix' <- lift $ Ixx.deleteDocsByURI d ix
    return (ix', ResOK)


execStore :: (Bin.Binary a, TextIndexerCon ix dt) =>
             FilePath -> a -> CM ix dt CmdResult
execStore filename x = do
    liftIO $ Bin.encodeFile filename x
    return ResOK


execLoad :: (Bin.Binary a, TextIndexerCon ix dt) =>
             FilePath -> CM ix dt (a, CmdResult)
execLoad filename = do
    x <- liftIO $ Bin.decodeFile filename
    return (x, ResOK)

-- ----------------------------------------------------------------------------

queryConfig     :: ProcessConfig
queryConfig     = ProcessConfig (FuzzyConfig True True 1.0 germanReplacements) True 100 500

runQueryM       :: TextIndexerCon ix dt
                => ContextIndex ix Occurrences
                -> Schema
                -> dt
                -> Query
                -> IO (Either CmdError (QRes.Result (Dt.DValue (Documents Document))))
runQueryM ix s dt q = processQuery st dt q
   where
   st = initState queryConfig ix s 2
