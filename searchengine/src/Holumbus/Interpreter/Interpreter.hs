{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Holumbus.Interpreter.Interpreter where

import           Control.Concurrent.MVar
import           Control.Monad.Error
import           Control.Monad.Reader

import           Data.Aeson
import qualified Data.Map as M

import           Holumbus.Index.InvertedIndex
import           Holumbus.DocTable.HashedDocuments
import           Holumbus.Index.Common (Position, DocId, Words, URI, Description, Document, Occurrences)
import           Holumbus.Index.Proxy.ContextIndex
import qualified Holumbus.Index.Index as Ix
import qualified Holumbus.Common.DocIdMap           as DM

import           Holumbus.Query.Fuzzy
import           Holumbus.Query.Language.Grammar
import           Holumbus.Query.Language.Parser
import           Holumbus.Query.Processor
import           Holumbus.Query.Result

import           Holumbus.Common.Document (unwrap)

import           Holumbus.DocTable.DocTable (DValue)
import qualified Holumbus.DocTable.DocTable as Dt

import qualified Holumbus.Common.Occurrences as Occ
-- ----------------------------------------------------------------------------

data Dummy
    = Dummy
      deriving (Show)

-- ----------------------------------------------------------------------------
--
-- the abstract syntax (syntactic domains)

{--
data InsOpts
    = New | Replace | Modify
      deriving (Show)
--}
--
data Command
    = Search     { _theQuery    :: String }
    | Completion { _thePrefix   :: String }
    | Insert     { _theRawDoc   :: Document
                 , _theOccs     :: Words -- change this later -> includ analyer here with options
 --                , _theInsOpts  :: InsOpts
                 }
    | Delete     { _theDocUri   :: URI }
    | LoadIx     { _thePath     :: FilePath }
    | StoreIx    { _thePath     :: FilePath }
    | Sequence   { _theCmdSeq   :: [Command] }
    | NOOP
    | MoreCommands
      deriving (Show)

data CmdRes
    = ResOK
    | ResSearch { _theDocs    :: [Document] }
    | ResCompl  { _theWords   :: [String] }
      deriving (Show)

data CmdError
    = ResError  { _theErr     :: Int
                , _theReason  :: String
                }
    deriving (Show)

instance Error CmdError where
    strMsg s = ResError 500 $ "internal server error: " ++ s

instance FromJSON Command     where parseJSON = undefined
--instance FromJSON InsOpts     where parseJSON = undefined

instance ToJSON Command       where toJSON = undefined
instance ToJSON CmdRes        where toJSON = undefined
instance ToJSON CmdError      where toJSON = undefined

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

type Indexer    = (ContextIndex InvertedIndex Occurrences, Documents Document)
type Options    = Dummy

data Env = Env
    { _theIndex      :: MVar Indexer
    , _theOptions    :: Options
    }

emptyIndexer    :: Indexer
emptyIndexer    = (Ix.empty, empty)

emptyOptions    :: Options
emptyOptions    = Dummy

initEnv :: Indexer -> Options -> IO Env
initEnv ixx@(ix,dt) opt
    = do ixref <- newMVar ixx
         return $ Env ixref opt

-- ----------------------------------------------------------------------------
--
-- the command evaluation monad

newtype CMT m a = CMT { runCMT :: ReaderT Env (ErrorT CmdError m) a }
  deriving (Monad, MonadIO, Functor, MonadReader Env, MonadError CmdError)

instance MonadTrans CMT where
  lift = CMT . lift . lift

type CM = CMT IO

runCmd :: Env -> Command -> IO (Either CmdError CmdRes)
runCmd env cmd
    = runErrorT . runReaderT (runCMT . execCmd $ cmd) $ env

askIx :: CM Indexer
askIx
    = do ref <- asks _theIndex
         liftIO $ readMVar ref

-- FIXME: io exception-safe?
modIx :: (Indexer -> CM (Indexer, a)) -> CM a
modIx f
    = do ref <- asks _theIndex
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

throwResError :: Int -> String -> CM a
throwResError n msg
    = throwError $ ResError n msg

-- ----------------------------------------------------------------------------

execCmd :: Command -> CM CmdRes
execCmd (Completion px)
    = withIx $ execCompletion px

execCmd (Search q)
    = withIx $ execSearch q

execCmd (Sequence cs)
    = execSequence cs

execCmd NOOP
    = return ResOK  -- keep alive test

execCmd (Insert doc ws)
    = do
      modIx $ execInsert doc ws
      withIx (\ix -> throwResError 333 $ show ix)

execCmd c
    = throwResError 501 $ "command not yet implemented: " ++ show c

-- ----------------------------------------------------------------------------

execCompletion :: String -> Indexer -> CM CmdRes
execCompletion px _ix
    = return $ ResCompl [px, px++"0"]

execSearch :: String -> Indexer -> CM CmdRes
execSearch q (ix,dt)
      =  case parseQuery q of
          (Left err) -> throwResError 502 "not implemented"
          (Right query) -> do
            res <- runQueryM ix dt query
            return $ ResSearch $ map (\(_,(DocInfo d _,_)) -> unwrap d) . DM.toList . docHits $ res

execSequence :: [Command] -> CM CmdRes
execSequence []       = execCmd NOOP
execSequence [c]      = execCmd c
execSequence (c : cs) = execCmd c >> execSequence cs

execInsert :: Document -> Words -> Indexer -> CM (Indexer,CmdRes)
execInsert doc wrds (ix,dt) = return ((newIndex, newDocTable),ResOK)
  where
  (did, newDocTable) = Dt.insert dt doc
  newIndex           = addWords wrds did ix 

-- ----------------------------------------------------------------------------
queryConfig     :: ProcessConfig
queryConfig     = ProcessConfig (FuzzyConfig True True 1.0 germanReplacements) True 100 500

runQueryM       :: ContextIndex InvertedIndex Occurrences
                -> Documents Document
                -> Query
                -> CMT IO (Holumbus.Query.Result.Result (Holumbus.DocTable.DocTable.DValue (Documents Document)))
runQueryM       = processQueryM queryConfig


-- | Add words for a document to the 'Index'.
addWords :: Words -> DocId -> ContextIndex InvertedIndex Occurrences -> ContextIndex InvertedIndex Occurrences
addWords wrds dId i 
  = M.foldrWithKey (\c wl acc ->
      M.foldrWithKey (\w ps acc' ->
        Ix.insert (Just c, Just w) (mkOccs dId ps) acc')
      acc wl)
      i wrds
  where
  mkOccs            :: DocId -> [Position] -> Occurrences
  mkOccs did pl     = positionsIntoOccs did pl Occ.empty

  positionsIntoOccs :: DocId -> [Position] -> Occurrences -> Occurrences
  positionsIntoOccs docId ws os = foldr (Occ.insert docId) os ws



test1 ws id = addWords ws id Ix.empty  
