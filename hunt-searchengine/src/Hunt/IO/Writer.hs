{-# LANGUAGE GADTs #-}
module Hunt.IO.Writer where

import           Data.Foldable
import           Data.Profunctor

-- | A writer is a moore automata taking input
--   modifying an internal state and an output function.
--   This gives us an interface for nicely composable writers.
data Writer m a b where
  W :: (m x) -> (x -> a -> m x) -> (x -> m b) -> Writer m a b

instance Functor m => Functor (Writer m a) where
  fmap f (W start step stop) = W start step (fmap f . stop)
  {-# INLINE fmap #-}

instance Functor m => Profunctor (Writer m) where
  dimap f g (W start step stop) =
    W start (\x s -> step x (f s)) (fmap g . stop)
  {-# INLINE dimap #-}

  rmap g (W start step stop) =
    W start step (fmap g . stop)
  {-# INLINE rmap #-}

  lmap f (W start step stop) =
    W start (\x s -> step x (f s)) stop
  {-# INLINE lmap #-}

data Pair a b = P !a !b

instance Applicative m => Applicative (Writer m a) where
  pure a = W (pure ()) (\() _ -> pure ()) (\() -> pure a)
  {-# INLINE pure #-}

  W fh fk fz <*> W ah ak az = W start step stop
    where start = P <$> fh <*> ah
          step (P f a) x = P <$> fk f x <*> ak a x
          stop (P f a) = fz f <*> az a
  {-# INLINE (<*>) #-}

runWriter :: (Monad m, Foldable f) => Writer m a b -> f a -> m b
runWriter (W start step stop) xs = do
  s <- start
  stop =<< foldlM step s xs
{-# INLINE runWriter #-}
