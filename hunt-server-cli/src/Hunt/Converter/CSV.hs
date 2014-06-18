module Hunt.Converter.CSV where

import           Prelude hiding ( mapM_, map, putStrLn)

import           Control.Applicative ((<$>))
import           Control.Monad.IO.Class  (MonadIO, liftIO)

import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson (Value(..))
import           Data.Aeson (ToJSON)

import           Data.ByteString.Lazy (ByteString)
import           Data.ByteString.Lazy.Char8  (putStrLn)

import           Data.Conduit (Consumer, Conduit, ($=), ($$))
import           Data.Conduit.Binary hiding ( mapM_)
import           Data.Conduit.List  (mapM_, map, mapAccum, consume)

import           Data.CSV.Conduit (defCSVSettings, intoCSV, MapRow, runResourceT)

import qualified Data.Map

import           Data.String.Conversions (cs)
import           Data.Text (Text)

import qualified Hunt.Common.DocDesc (DocDesc, fromList)
import qualified Hunt.ClientInterface as H

mapToHashMap :: Data.Map.Map Text Text -> Hunt.Common.DocDesc.DocDesc -- Data.HashMap.HashMap Text Value
mapToHashMap src = Hunt.Common.DocDesc.fromList $ ((\(k,v) -> (k, String v))<$> Data.Map.toList src)

rowToApiDocument :: (Monad m) => FilePath -> Conduit (MapRow Text) m (H.ApiDocument)
rowToApiDocument fileName = mapAccum convertDoc 0 >> return ()
    where
    convertDoc :: MapRow Text -> Int -> (Int, H.ApiDocument)
    convertDoc row i = (i+1, H.setDescription (mapToHashMap row) $ H.setIndex row $ H.mkApiDoc $ cs $ "file://" ++ fileName ++ "/" ++ (show i))

mapToJson :: (ToJSON j, Monad m) => Conduit j m (ByteString)
mapToJson = map encodePretty

printSink :: (MonadIO m) => Consumer ByteString m ()
printSink = mapM_ (liftIO . putStrLn )

convert :: FilePath -> IO ()
convert fileName = do
    list <- runResourceT $
      sourceFile fileName $=
      intoCSV defCSVSettings $=
      rowToApiDocument fileName $$
      consume
    putStrLn $ encodePretty list

