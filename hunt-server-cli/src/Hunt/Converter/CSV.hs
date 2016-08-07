module Hunt.Converter.CSV where

import           Control.Monad.Except
import           Control.Monad.Trans.Resource
import           Data.Aeson.Encode.Pretty     (encodePretty)
import           Data.ByteString.Lazy.Char8   (putStrLn)
import           Data.Conduit                 (Conduit, ($$), ($=))
import           Data.Conduit.Binary          hiding (mapM_)
import           Data.Conduit.List            (consume)
import qualified Data.Csv.Conduit             as CCSV
import qualified Data.Csv.Parser              as CSV
import           Data.Map                     (Map)
import qualified Data.Map
import           Data.String.Conversions      (cs)
import           Data.Text                    (Text)
import qualified Hunt.ClientInterface         as H
import qualified Hunt.Conduit                 as HC
import           Prelude                      hiding (map, mapM_, putStrLn)

convertToDocument :: FilePath -> Int -> Map Text Text -> H.ApiDocument
convertToDocument fileName i row = H.listToApiDoc uri elems elems
    where
    uri = cs $ "file://" ++ fileName ++ "/" ++ (show i)
    elems = Data.Map.toList row

rowToApiDocument :: Monad m => FilePath -> Conduit (Map Text Text) m H.Command
rowToApiDocument fileName = HC.makeInsertsWithIndex (convertToDocument fileName)

convert :: FilePath -> IO ()
convert fileName = do
  elist <- runResourceT $ runExceptT $
    sourceFile fileName
    $= CCSV.fromNamedCsv CSV.defaultDecodeOptions
    $= rowToApiDocument fileName
    $$ consume

  case elist of
    Right list                     -> putStrLn $ encodePretty list
    Left (CCSV.CsvParseError _ e)  -> print e
    Left (CCSV.IncrementalError e) -> print e
