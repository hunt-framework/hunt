{-# LANGUAGE RankNTypes #-}
module Hunt.Interpreter.Worker where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.XMVar
import           Control.Monad
import           Control.Monad.IO.Class

-- | A lightweight abstraction for a pool of asynchronous threads
--   which, when tickled, do some task over a given XMVar.
--   Note that a worker forms a monoid with the noop as neutral element
--   which can be handy when composing multiple workers.
newtype Worker = Worker { runWorker :: IO () }

instance Monoid Worker where
  mempty = Worker (return ())
  mappend a b = Worker (runWorker a >> runWorker b)

new :: (MonadIO m)
    => Int
    -> XMVar a
    -> (IO a -> (forall b. (a -> IO (a, b)) -> IO b) -> IO ())
    -> m Worker
new nworkers xmvar action  = liftIO $ do
  ch <- newEmptyTMVarIO
  replicateM_ nworkers $ forkIO $
    forever $ do
      _ <- atomically (takeTMVar ch)
      action
        (readXMVar xmvar)
        (modifyXMVar xmvar)

  return $ Worker $ do
    _ <- atomically $ tryPutTMVar ch ()
    return ()

tickle :: MonadIO m => Worker -> m ()
tickle = liftIO . runWorker
