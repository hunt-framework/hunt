{-# LANGUAGE ExistentialQuantification #-}
module Hunt.IO.Writer where

import Data.Foldable

-- | 'Writer' is an abstraction for composable loops.
-- For example one can have a 'Writer' which emits high-level
-- records, one which buffers its output and one which writes
-- the buffer to disk.
data Writer a b = forall x. WR (IO x) (x -> a -> IO x) (x -> IO b)

instance Functor (Writer a) where
  fmap f (WR start step stop) = WR start step (fmap f . stop)
  {-# INLINE fmap #-}

data T2 a b = T2 !a !b

data T3 a b c = T3 !a !b !c

data T4 a b c d = T4 !a !b !c !d

instance Applicative (Writer a) where
  pure a = WR (pure ()) (\_ _ -> pure ()) (\_ -> pure a)
  {-# INLINE pure #-}

  WR h k z <*> WR m n o = WR
    (T2 <$> h <*> m)
    (\(T2 a b) x -> T2 <$> k a x <*> n b x )
    (\(T2 a b) -> z a <*> o b)
  {-# INLINE (<*>) #-}

runWriter :: Foldable f => Writer a b -> f a -> IO b
runWriter (WR start step stop) as = do
  s <- start
  stop =<< foldlM step s as
{-# INLINE runWriter #-}
