module Hunt.Converter.CSV where

import           Prelude hiding ( mapM_, map, putStrLn)

import           Control.Applicative ((<$>))

import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson (Value(..))

import           Data.ByteString.Lazy.Char8  (putStrLn)

import           Data.Conduit (Conduit, ($=), ($$))
import           Data.Conduit.Binary hiding ( mapM_)
import           Data.Conduit.List  (consume)

import           Data.CSV.Conduit (defCSVSettings, intoCSV, MapRow, runResourceT)

import qualified Data.Map

import           Data.String.Conversions (cs)
import           Data.Text (Text)

import qualified Hunt.Common.DocDesc (DocDesc, fromList)
import qualified Hunt.ClientInterface as H
import qualified Hunt.Conduit as HC

convertToDocument :: FilePath -> Int -> MapRow Text -> H.ApiDocument
convertToDocument fileName i row = H.listToApiDoc uri elems elems
    where
    uri = (cs $ "file://" ++ fileName ++ "/" ++ (show i))
    elems = (Data.Map.toList row)

rowToApiDocument :: (Monad m) => FilePath -> Conduit (MapRow Text) m H.Command
rowToApiDocument fileName = HC.makeInsertsWithIndex (convertToDocument fileName)


convert :: FilePath -> IO ()
convert fileName = do
    list <- runResourceT $
      sourceFile fileName $=
      intoCSV defCSVSettings $=
      rowToApiDocument fileName $$
      consume
    putStrLn $ encodePretty list

