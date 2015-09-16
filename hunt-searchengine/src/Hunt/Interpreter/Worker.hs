{-# LANGUAGE RankNTypes #-}
module Hunt.Interpreter.Worker where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.XMVar
import           Control.Monad
import           Control.Monad.IO.Class

newtype Worker = Worker (TMVar ())

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
  return (Worker ch)

tickle :: MonadIO m => Worker -> m ()
tickle (Worker chan) = liftIO $ atomically $ do
  _ <- tryPutTMVar chan ()
  return ()
