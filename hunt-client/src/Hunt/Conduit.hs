{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Hunt.Conduit where

import           Prelude hiding ( mapM_, map, putStrLn)
import           Control.Monad.Primitive (PrimMonad)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson (Value, ToJSON)
import           Data.ByteString.Lazy (ByteString)
import           Data.ByteString.Lazy.Char8  (putStrLn)
import           Data.Conduit.List  (mapM_, map, mapAccum, consume)
import           Conduit
import qualified Data.Vector as Vector
import           Hunt.Server.Client
import qualified Hunt.ClientInterface as H

instance (MonadBase b m) => MonadBase b (HuntConnectionT m) where
  liftBase = lift . liftBase

makeInsertsWithIndex :: (Monad m) => (Int -> i -> H.ApiDocument) -> Conduit i m H.Command
makeInsertsWithIndex f = (mapAccum convertDoc 0) >> return ()
    where
    convertDoc row i = (i+1, H.cmdInsertDoc $ f i row)

makeInserts :: (Monad m) => (i -> H.ApiDocument) -> Conduit i m H.Command
makeInserts f = makeInsertsWithIndex (const f)

mergeInserts :: (Monad m) => Consumer H.Command m H.Command
mergeInserts = H.cmdSequence <$> consume

rechunkCommands :: (MonadBase b m, PrimMonad b)
             => Int
             -> Conduit H.Command m H.Command
rechunkCommands sz =
  conduitVector sz =$= mapC (H.cmdSequence . Vector.toList)

printInserts :: (MonadIO m) => Consumer H.Command m ()
printInserts = mapToJson $= printSink

mapToJson :: (ToJSON j, Monad m) => Conduit j m (ByteString)
mapToJson = map encodePretty

printSink :: (MonadIO m) => Consumer ByteString m ()
printSink = mapM_ (liftIO . putStrLn )

cmdSink :: (MonadIO m, MonadThrow m) => Consumer H.Command (HuntConnectionT m) ()
cmdSink = mapM_ (\cmd -> do
                    _ :: Value <- postCommand cmd
                    return ())
