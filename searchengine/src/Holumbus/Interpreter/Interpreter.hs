{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Holumbus.Interpreter.Interpreter where

import           Control.Concurrent.MVar
import           Control.Monad.Error
import           Control.Monad.Reader

import           Data.Set                          (Set)
import qualified Data.Set                          as S
import           Data.Text                         (Text)
import qualified Data.Text                         as T


import           Holumbus.Common.BasicTypes        
import           Holumbus.Common.ApiDocument       as ApiDoc
import           Holumbus.Common.DocIdMap          (DocIdSet, toDocIdSet)
import           Holumbus.Common.Occurrences       (Occurrences)
import           Holumbus.Common.Document          (Document, unwrap)
import qualified Holumbus.Common.DocIdMap          as DM

import           Holumbus.Analyzer.Analyzer

import qualified Holumbus.Index.Index              as Ix
import           Holumbus.Index.InvertedIndex
import           Holumbus.Index.Proxy.ContextIndex (ContextIndex)
import qualified Holumbus.Index.Proxy.ContextIndex as CIx
import qualified Holumbus.Index.TextIndex          as TIx

import           Holumbus.Query.Fuzzy
import           Holumbus.Query.Language.Grammar
import           Holumbus.Query.Processor
import           Holumbus.Query.Result             as QRes

import qualified Holumbus.DocTable.DocTable        as Dt
import           Holumbus.DocTable.HashedDocuments as HDt

import           Holumbus.Utility                  (catMaybesSet)

-- ----------------------------------------------------------------------------
--
-- the semantic domains (datatypes for interpretation)
--
-- Env, Index, ...

-- ----------------------------------------------------------------------------
--
-- the environment
-- with a MVar for storing the index
-- so the MVar acts as a global state (within IO)

type Indexer
    = (ContextIndex InvertedIndex Occurrences, Documents Document)

emptyIndexer    :: Indexer
emptyIndexer    = (CIx.empty, HDt.empty)

-- ----------------------------------------------------------------------------

{-
data Options = Options
    { opSplitter :: ApiDocument -> (Document, Words)
    }

emptyOptions :: Options
emptyOptions = Options
    { opSplitter = toDocAndWords
    }
-}

data Options = Options

emptyOptions :: Options
emptyOptions = Options

-- ----------------------------------------------------------------------------

data Env = Env
    { evIndexer :: MVar Indexer
    , evOptions :: Options
    }

initEnv :: Indexer -> Options -> IO Env
initEnv ixx opt
    = do ixref <- newMVar ixx
         return $ Env ixref opt

-- ----------------------------------------------------------------------------
-- the command evaluation monad
-- ----------------------------------------------------------------------------
newtype CMT m a = CMT { runCMT :: ReaderT Env (ErrorT CmdError m) a }
  deriving (Monad, MonadIO, Functor, MonadReader Env, MonadError CmdError)

instance MonadTrans CMT where
  lift = CMT . lift . lift

type CM = CMT IO

-- ----------------------------------------------------------------------------

runCM :: CMT m a -> Env -> m (Either CmdError a)
runCM env = runErrorT . runReaderT (runCMT $ env)

runCmd :: Env -> Command -> IO (Either CmdError CmdResult)
runCmd env cmd
    = runErrorT . runReaderT (runCMT . execCmd $ cmd) $ env

askIx :: CM Indexer
askIx
    = do ref <- asks evIndexer
         liftIO $ readMVar ref

-- FIXME: io exception-safe?
modIx :: (Indexer -> CM (Indexer, a)) -> CM a
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

modIx_ :: (Indexer -> CM Indexer) -> CM ()
modIx_ f = modIx f'
    where f' i = f i >>= \r -> return (r, ())

withIx :: (Indexer -> CM a) -> CM a
withIx f
    = askIx >>= f

askOpts :: CM Options
askOpts
    = asks evOptions

throwResError :: Int -> Text -> CM a
throwResError n msg
    = throwError $ ResError n msg

throwNYI :: String -> CM a
throwNYI c = throwResError 501 $ "command not yet implemented: " `T.append` (T.pack c)

-- ----------------------------------------------------------------------------

execCmd :: Command -> CM CmdResult
execCmd (Search q)
    = withIx $ execSearch $ q

execCmd (Sequence cs)
    = execSequence cs

execCmd NOOP
    = return ResOK  -- keep alive test

execCmd (Insert doc opts)
    = modIx $ execInsert doc opts

execCmd (Delete uri)
    = modIx $ execDelete uri

execCmd c
    = throwNYI $ show c

-- ----------------------------------------------------------------------------

execSearch :: Query -> Indexer -> CM CmdResult
execSearch q (ix, dt) = do
    res <- runQueryM ix dt q
    return $ ResSearch $ map (\(_, (DocInfo d _, _)) -> unwrap d) . DM.toList . docHits $ res


execSequence :: [Command] -> CM CmdResult
execSequence []       = execCmd NOOP
execSequence [c]      = execCmd c
execSequence (c : cs) = execCmd c >> execSequence cs


execInsert :: ApiDocument -> InsertOption -> Indexer -> CM (Indexer, CmdResult)
execInsert doc op ixx = do
    --split <- asks (opSplitter . evOptions)
    let split = toDocAndWords
    let (docs, ws) = split doc
    let ix'        = insert docs ws ixx
    case op of
        New     -> return (ix', ResOK) -- TODO: not the real deal yet
        x       -> throwNYI $ show x


execDelete :: URI -> Indexer -> CM (Indexer, CmdResult)
execDelete d ix = do
    let ix' = deleteDocsByURI (S.singleton d) ix
    return (ix', ResOK)

-- ----------------------------------------------------------------------------
-- Indexer functions
-- ----------------------------------------------------------------------------

-- | Insert a Document and Words.
insert :: Document -> Words -> Indexer -> Indexer
insert doc wrds (ix,dt)
    = (newIx, newDt)
    where
    (did, newDt) = Dt.insert dt doc
    newIx        = TIx.addWords wrds did ix

-- | Delete a set if documents by 'URI'.
deleteDocsByURI :: Set URI -> Indexer -> Indexer
deleteDocsByURI us ixx@(_ix,dt)
    = delete ixx docIds
    where
    docIds = toDocIdSet . catMaybesSet . S.map (Dt.lookupByURI dt) $ us

-- | Delete a set of documents by 'DocId'.
delete :: Indexer -> DocIdSet -> Indexer
delete (ix,dt) dIds
  = (newIx, newDt)
    where
    newIx = CIx.map (Ix.batchDelete dIds) ix
    newDt = Dt.difference dIds            dt

-- ----------------------------------------------------------------------------

queryConfig     :: ProcessConfig
queryConfig     = ProcessConfig (FuzzyConfig True True 1.0 germanReplacements) True 100 500

runQueryM       :: ContextIndex InvertedIndex Occurrences
                -> Documents Document
                -> Query
                -> CM (QRes.Result (Dt.DValue (Documents Document)))
runQueryM       = processQueryM queryConfig
