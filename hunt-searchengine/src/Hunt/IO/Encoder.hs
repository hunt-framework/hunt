{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
module Hunt.IO.Encoder (
    Encoding
  , (>*<)
  , (>$<)
  , varint
  , word64BE
  , byteString
  , lazyByteString

  , Writer(..)
  , runWriter
  , runWriterWith
  , appendWriter
  , bufferedEncWriter
  , bufferedEncWriter'
  , bufferedByteStringWriter
  , lazyByteStringWriter

  , Pair(..)
  ) where

import           Hunt.IO.Buffer           (Buffer)
import qualified Hunt.IO.Buffer           as Buffer
import qualified Hunt.IO.File             as File

import           Data.Bits
import           Data.ByteString          (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString
import qualified Data.ByteString.Lazy     as LBS
import           Data.Foldable
import           Data.Profunctor
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable

data Encoding a = E !Int (a -> Ptr Word8 -> IO (Ptr Word8))

pairE :: Encoding a -> Encoding b -> Encoding (a, b)
pairE (E n1 enc1) (E n2 enc2) = E (n1 + n2) $ \(a, b) op ->
  enc1 a op >>= enc2 b
{-# INLINE CONLIkE pairE #-}

class Monoidal f where
  pair :: f a -> f b -> f (a, b)

instance Monoidal Encoding where
  pair = pairE
  {-# INLINE CONLIKE pair #-}

(>*<) :: Monoidal f => f a -> f b -> f (a, b)
(>*<) = pair
{-# INLINE CONLIKE (>*<) #-}
infixr 5 >*<

class Contravariant f where
  cmap :: (b -> a) -> f a -> f b

instance Contravariant Encoding where
  cmap f (E n g) = E n (g . f)
  {-# INLINE cmap #-}

(>$<) :: Contravariant f => (b -> a) -> f a -> f b
(>$<) = cmap
{-# INLINE (>$<) #-}
infixl 4 >$<

word64BE :: Encoding Word64
word64BE = E 8 $ \n op -> do
  undefined
{-# INLINE word64BE #-}

varint :: Encoding Word64
varint = E 9 go
  where go :: Word64 -> Ptr Word8 -> IO (Ptr Word8)
        go !n !op
          | n < 0x80 = do poke op (fromIntegral n)
                          return (op `plusPtr` 1)
          | otherwise = do poke op (setBit (fromIntegral n) 7)
                           go (n `unsafeShiftR` 7) (op `plusPtr` 1)
{-# INLINE varint #-}

byteString :: Int -> Encoding ByteString
byteString n = E n step
  where step :: ByteString -> Ptr Word8 -> IO (Ptr Word8)
        step (ByteString.PS fop off len) op =
          withForeignPtr fop $ \ptr -> do
            ByteString.memcpy op (ptr `plusPtr` off) (min n len)
            return $ op `plusPtr` min n len
{-# INLINE byteString #-}

lazyByteString :: Int -> Encoding LBS.ByteString
lazyByteString n = E n step
  where step :: LBS.ByteString -> Ptr Word8 -> IO (Ptr Word8)
        step lbs op = do
          undefined
{-# INLINE lazyByteString #-}

data Writer m a b where
  W :: m x -> (x -> a -> m x) -> (x -> m b) -> Writer m a b

instance Functor m => Functor (Writer m a) where
  fmap f (W start step stop) = W start step (fmap f . stop)
  {-# INLINE fmap #-}

instance Functor m => Profunctor (Writer m) where
  lmap f (W start step stop) =
    W start (\x a -> step x (f a)) stop
  {-# INLINE lmap #-}

  rmap f (W start step stop) =
    W start step (fmap f . stop)
  {-# INLINE rmap #-}

  dimap f g (W start step stop) =
    W start (\x a -> step x (f a)) (fmap g . stop)
  {-# INLINE dimap #-}

runWriter :: (Monad m, Foldable f) => Writer m a b -> f a -> m b
runWriter w as = runWriterWith foldlM w as
{-# INLINE runWriter #-}

runWriterWith :: Monad m
              => (forall c. (c -> a -> m c) -> c -> t -> m c)
              -> Writer m a b
              -> t
              -> m b
runWriterWith foldlM' (W start step stop) as = do
  s <- start
  stop =<< foldlM' step s as
{-# INLINE runWriterWith #-}

data Pair a b = P !a !b

instance Applicative m => Applicative (Writer m a) where
  pure a = W (pure ()) (\_ _ -> pure ()) (\_ -> pure a)
  {-# INLINE pure #-}

  W h k z <*> W s t u = W
    (P <$> h <*> s)
    (\(P a b) x -> P <$> k a x <*> t b x)
    (\(P a b) -> z a <*> u b)
  {-# INLINE (<*>) #-}

appendWriter :: File.AppendFile
             -> Writer IO ByteString Word64
appendWriter appFile = W start step stop
  where
    start = pure 0

    step !n bytes = do
      bytesWritten <- File.append appFile bytes
      return (fromIntegral bytesWritten + n)

    stop !n = pure n
{-# INLINE appendWriter #-}

data BWS a = BWS !Buffer !a

bufferedEncWriter' :: forall a b. Int
                -> Writer IO ByteString b
                -> Writer IO (Pair (Encoding a) a) b
bufferedEncWriter' bufSz writer =
  case writer of
    W wstart wstep wstop -> W start step stop where

      start = BWS <$> Buffer.newBuffer bufSz
                  <*> wstart

      step (BWS buf ws) (P (E encSz runEncoding) x)
        | Buffer.hasEnoughBytes buf encSz = do
            buf' <- Buffer.put buf (runEncoding x)
            return $ BWS buf' ws
        | otherwise = do
            ws' <- wstep ws (Buffer.toByteString buf)
            buf' <- Buffer.put (Buffer.reset buf) (runEncoding x)
            return $ BWS buf' ws'

      stop (BWS buf ws)
        | not (Buffer.null buf) = do
            ws' <- wstep ws (Buffer.toByteString buf)
            wstop ws'
        | otherwise = wstop ws
{-# INLINE bufferedEncWriter' #-}

bufferedEncWriter :: Int
               -> Encoding a
               -> Writer IO ByteString b
               -> Writer IO a b
bufferedEncWriter bufSz enc writer =
  case bufferedEncWriter' bufSz writer of
    W wstart wstep wstop -> W start step stop where
      start = wstart
      step s a = wstep s (P enc a)
      stop s = wstop s
{-# INLINE bufferedEncWriter #-}

bufferedByteStringWriter :: Int
                         -> Writer IO ByteString b
                         -> Writer IO ByteString b
bufferedByteStringWriter bufSz writer = case writer of
  W wstart wstep wstop -> W start step stop where

    start = BWS <$> Buffer.newBuffer bufSz
                <*> wstart

    step (BWS buf ws) bs
      | Buffer.hasEnoughBytes buf (ByteString.length bs) = do
          buf' <- Buffer.putByteString buf bs
          return $ BWS buf' ws
      | otherwise = do
          ws' <- wstep ws (Buffer.toByteString buf)
          buf' <- Buffer.putByteString (Buffer.reset buf) bs
          return $ BWS buf' ws'

    stop (BWS buf ws)
      | not (Buffer.null buf) = do
          ws' <- wstep ws (Buffer.toByteString buf)
          wstop ws'
      | otherwise = wstop ws
{-# INLINE bufferedByteStringWriter #-}

lazyByteStringWriter :: Writer IO ByteString b
                     -> Writer IO LBS.ByteString b
lazyByteStringWriter writer = case writer of
  W wstart wstep wstop -> W start step stop where
    start = wstart
    step ws0 lbs = foldlM wstep ws0 (LBS.toChunks lbs)
    stop = wstop
{-# INLINE lazyByteStringWriter #-}
