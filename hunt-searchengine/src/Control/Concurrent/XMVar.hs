-- ----------------------------------------------------------------------------
{- |
  An 'MVar' variation that only blocks for modification.
  Readers are never blocked but write access is carried out in sequence.

  This is done with two 'MVar's.
  While modification is done, the readers use the old value.
  When the modification is done, the old (unmodified) value is replaced with the new one.
  For this to work, the writers have to block each other which is done with the second 'MVar'.
  This process is encapsulated in 'modifyXMVar' and 'modifyXMVar_'.

  /Note/: This may increase the memory usage since there may be two value present at a time.
          This is intended to be used with (big) data structures where small changes are made.
-}
-- ----------------------------------------------------------------------------

module Control.Concurrent.XMVar
  ( XMVar
  , newXMVar
  , readXMVar, modifyXMVar, modifyXMVar_
  , takeXMVarWrite, putXMVarWrite
  )
where

import           Control.Concurrent.MVar
import           Control.Exception

-- ------------------------------------------------------------

-- | An 'MVar' variation that only blocks for modification.
--   It consists of two 'MVar's. One for the value which can always be read and the second one
--   to block writers so that modifications are done sequentially.
data XMVar a = XMVar (MVar a) (MVar ())

-- ------------------------------------------------------------

-- | Create a new 'XMVar' with the supplied value.
newXMVar :: a -> IO (XMVar a)
newXMVar v = do
  m <- newMVar v
  l <- newMVar ()
  return $ XMVar m l

-- | Read the value.
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

-- | Locks for writes and reads the value. Readers do not block each other.
--   'modifyXMVar' encapsulates 'takeXMVarWrite' and 'putXMVarWrite' and also handles exceptions.
takeXMVarWrite :: XMVar a -> IO a
takeXMVarWrite (XMVar m l)
  = takeMVar l >> readMVar m

-- | Replaces the value (since it was locked for potential writers) and unlocks writers.
putXMVarWrite :: XMVar a -> a -> IO ()
putXMVarWrite (XMVar m l) v
  = swapMVar m v >> putMVar l ()

-- ------------------------------------------------------------
