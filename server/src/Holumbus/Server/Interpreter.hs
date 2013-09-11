module Holumbus.Server.Interpreter where

import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Concurrent.MVar

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Set              as S
import           Holumbus.Indexer.Indexer as Ix
import           Holumbus.Server.Common

import           Holumbus.Index.Common.BasicTypes

-- ----------------------------------------------------------------------------
-- Interpreter
-- ----------------------------------------------------------------------------

data CMOptions de e = CMOptions
  { ceoSplit :: ApiDocument -> (de, e)
  }

data CMEnv e it iv i d de = CMEnv
    { cevIndexer    :: MVar (Indexer e it iv i d de)
    , cevOptions    :: CMOptions de e
    }

initEnv :: Indexer e it iv i d de -> CMOptions de e -> IO (CMEnv e it iv i d de)
initEnv ix opt
    = do ixref <- newMVar ix
         return $ CMEnv ixref opt

newtype CMT e it iv i d de m a = CMT { runCMT :: ReaderT (CMEnv e it iv i d de) (ErrorT CmdError m) a }
  deriving (Monad, MonadIO, Functor, MonadReader (CMEnv e it iv i d de), MonadError CmdError)

instance MonadTrans (CMT e it iv i d de) where
  lift = CMT . lift . lift

type CM e it iv i d de a = (CMT e it iv i d de IO a)

runCmd :: CMEnv e it iv i d de -> Command -> IO (Either CmdError CmdResult)
runCmd env cmd
    = runErrorT . runReaderT (runCMT . execCmd $ cmd) $ env

askIx :: CM e it iv i d de (Indexer e it iv i d de)
askIx
    = do ref <- asks cevIndexer
         liftIO $ readMVar ref

-- FIXME: io exception-safe?
modIx :: (Indexer e it iv i d de -> CM e it iv i d de (Indexer e it iv i d de, a)) -> CM e it iv i d de a
modIx f
    = do ref <- asks cevIndexer
         i <- liftIO $ takeMVar ref
         (i', a) <- f i `catchError` putBack ref i
         liftIO $ putMVar ref i'
         return a
    where
    putBack ref i e = do
        liftIO $ putMVar ref i
        throwError e

-- XXX: mk pretty
modIx_ :: (Indexer e it iv i d de -> CM e it iv i d de (Indexer e it iv i d de)) -> CM e it iv i d de ()
modIx_ f = modIx f'
    where f' i = f i >>= \r -> return (r, ())

askOpts :: CM e it iv i d de (CMOptions de e)
askOpts
    = asks cevOptions

withIx :: (Indexer e it iv i d de -> CM e it iv i d de a) -> CM e it iv i d de a
withIx f
    = askIx >>= f

throwResError :: Int -> Text -> CM e it iv i d de a
throwResError n msg
    = throwError $ ResError n msg

throwNYI :: String -> CM e it iv i d de a
throwNYI c = throwResError 501 $ "command not yet implemented: " `T.append` (T.pack c)

-- ----------------------------------------------------------------------------
{-
    NOOP
    Sequence cs
    Insert d op
    Delete u

    -- TODO:
    -- probably needs a mkIndex function (Env) - may become obsolete with type classes?
    LoadIx  f
    StoreIx f

    -- query stuff needs work
    Search q
    Completion s
-}

execCmd :: Command -> CM e it iv i d de CmdResult
execCmd NOOP
  = return ResOK -- keep alive test

execCmd (Sequence cs)
  = execSequence cs

execCmd (Insert d op)
  = modIx $ execInsert d op

execCmd (Delete d)
  = modIx $ execDelete d

execCmd c
  = throwNYI . show $ c

-- ----------------------------------------------------------------------------

execSequence :: [Command] -> CM e it iv i d de CmdResult
execSequence []       = execCmd NOOP
execSequence [c]      = execCmd c
execSequence (c : cs) = execCmd c >> execSequence cs

execInsert :: ApiDocument -> InsertOption -> Indexer e it iv i d de -> CM e it iv i d de (Indexer e it iv i d de, CmdResult)
execInsert d op ix = do
    split <- asks (ceoSplit . cevOptions)
    let (docs, ws) = split d
    let ix' = Ix.insert ix docs ws
    case op of
        New     -> return (ix', ResOK) -- TODO: not the real deal yet
        x       -> throwNYI . show $ x

execDelete :: URI -> Indexer e it iv i d de -> CM e it iv i d de (Indexer e it iv i d de, CmdResult)
execDelete d ix = do
    let ix' = Ix.deleteDocsByURI (S.singleton d) ix
    return (ix', ResOK)
