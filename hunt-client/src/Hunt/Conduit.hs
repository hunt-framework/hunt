module Hunt.Conduit where

import           Prelude hiding ( mapM_, map, putStrLn)

import           Control.Applicative ((<$>))
import           Control.Monad.IO.Class  (MonadIO, liftIO)

import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson (ToJSON)

import           Data.ByteString.Lazy (ByteString)
import           Data.ByteString.Lazy.Char8  (putStrLn)

import           Data.Conduit (Consumer, Conduit, ($=))
import           Data.Conduit.List  (mapM_, map, mapAccum, consume)

import qualified Hunt.ClientInterface as H

makeInsertsWithIndex :: (Monad m) => (Int -> i -> H.ApiDocument) -> Conduit i m H.Command
makeInsertsWithIndex f = (mapAccum convertDoc 0) >> return ()
    where
    convertDoc row i = (i+1, H.cmdInsertDoc $ f i row)

makeInserts :: (Monad m) => (i -> H.ApiDocument) -> Conduit i m H.Command
makeInserts f = makeInsertsWithIndex (const f)

mergeInserts :: (Monad m) => Consumer H.Command m H.Command
mergeInserts = H.cmdSequence <$> consume

printInserts :: (MonadIO m) => Consumer H.Command m ()
printInserts = mapToJson $= printSink

mapToJson :: (ToJSON j, Monad m) => Conduit j m (ByteString)
mapToJson = map encodePretty

printSink :: (MonadIO m) => Consumer ByteString m ()
printSink = mapM_ (liftIO . putStrLn )
