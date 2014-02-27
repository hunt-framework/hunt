module Control.Concurrent.XMVar
  ( XMVar
  , newXMVar
  , readXMVar, modifyXMVar, modifyXMVar_
  , takeXMVarWrite, putXMVarWrite
  )
where

import           Control.Concurrent.MVar
import           Control.Exception

-- ----------------------------------------------------------------------------

data XMVar a = XMVar (MVar a) (MVar ())

-- ----------------------------------------------------------------------------

-- | Creates a new 'XMVar' with the supplied value.
newXMVar :: a -> IO (XMVar a)
newXMVar v = do
  m <- newMVar v
  l <- newMVar ()
  return $ XMVar m l

-- | Reads the value.
readXMVar :: XMVar a -> IO a
readXMVar (XMVar m _)
  = readMVar m

-- | Modify the content.
modifyXMVar :: XMVar a -> (a -> IO (a, b)) -> IO b
modifyXMVar (XMVar m l) f
  = mask $ \restore -> do
    _ <- takeMVar l
    v <- readMVar m
    (v',a) <- restore (f v) `onException` putMVar l ()
    _ <- swapMVar m v'
    putMVar l ()
    return a

-- | Like 'modifyXMVar' but without a return value.
modifyXMVar_ :: XMVar a -> (a -> IO a) -> IO ()
modifyXMVar_ (XMVar m l) f
  = mask $ \restore -> do
    _  <- takeMVar l
    v  <- readMVar m
    v' <- restore (f v) `onException` putMVar l ()
    _  <- swapMVar m v'
    putMVar l ()

-- | Locks for writers and reads the value. Readers do not block each other.
takeXMVarWrite :: XMVar a -> IO a
takeXMVarWrite (XMVar m l)
  = takeMVar l >> readMVar m

-- | Replaces the value (since it was locked for potential writers) and unlocks writers.
putXMVarWrite :: XMVar a -> a -> IO ()
putXMVarWrite (XMVar m l) v
  = swapMVar m v >> putMVar l ()
