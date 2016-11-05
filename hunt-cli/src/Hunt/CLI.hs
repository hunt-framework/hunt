module Hunt.CLI
  ( P.huntCLI
  , runCommand
  ) where

import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Csv                   as CSV
import           Data.Either                (either)
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import qualified Hunt.API                   as HC
import qualified Hunt.CLI.Parser            as P
import           Hunt.CLI.Types
import qualified Hunt.Client                as HC
import qualified Hunt.ClientInterface       as HC
import           Hunt.Common.ApiDocument    (LimitedResult)
import           Hunt.Interpreter
import qualified Hunt.Interpreter.Command   as I
import           Hunt.Query.Intermediate    (RankedDoc)
import           Network.HTTP.Client
import           Servant.Client             (BaseUrl, ClientM)


-- EXECUTING COMMANDS

-- | A Cmd either entails an CliErr or
-- some result to be executed in the IO monad
type Cmd = ExceptT CliErr IO


runCommand :: CliCommand -> IO ()
runCommand command =
  case command of
    Eval opts file ->
      printCmd (putStrLn . show) $ eval opts file

    Search opts offset limit query ->
      printCmd printJson $ search opts offset limit query

    Completion opts query ->
      printCmd printJson $ complete opts query

    MakeSchema file ->
      printCmd printJson $ makeSchema file

    MakeInsert file ->
      printCmd printJson $ makeInsert file

    FromCSV file ->
      printCmd printJson $ fromCsv file


-- OPERATIONS

eval :: ServerOptions -> FilePath -> Cmd I.CmdResult
eval opts file = readAll file >>= decodeJson >>= request opts . HC.eval


search :: ServerOptions -> Maybe Offset -> Maybe Limit -> T.Text -> Cmd (LimitedResult RankedDoc)
search opts offset limit query =
  request opts $ HC.search query offset limit


complete :: ServerOptions -> T.Text -> Cmd HC.Suggestion
complete opts = request opts . HC.completeAll


makeSchema :: FilePath -> Cmd I.Command
makeSchema file = do
  readAll file >>= decodeApiDocs >>= return . HC.createContextCommands


makeInsert :: FilePath -> Cmd I.Command
makeInsert file = readAll file >>= decodeApiDocs >>= insertDocs
  where insertDocs = return . HC.cmdSequence . fmap HC.cmdInsertDoc


fromCsv :: FilePath -> Cmd ()
fromCsv = undefined
  where
    eval :: LBS.ByteString -> Either String (CSV.Header, V.Vector (M.Map T.Text T.Text))
    eval input = undefined


-- HELPERS

printCmd :: (a -> IO ()) -> Cmd a -> IO ()
printCmd f cmd = runExceptT cmd >>= either printErr f
  where printErr = putStrLn . formatErr


printJson :: ToJSON a => a -> IO ()
printJson = LBS.putStrLn . encodePretty


readAll :: FilePath -> Cmd LBS.ByteString
readAll = lift . LBS.readFile


decodeJson :: FromJSON a => LBS.ByteString -> Cmd a
decodeJson = either throw return . eitherDecode
  where throw = throwError . JsonErr


request :: ServerOptions -> ClientM a -> Cmd a
request baseUrl req = do
  client <- lift $ HC.withBaseUrl baseUrl
  result <- lift $ HC.runClientM req client
  either (throwError . HttpErr) return result


decodeApiDocs :: LBS.ByteString -> Cmd [HC.ApiDocument]
decodeApiDocs json = HC.insertCmdsToDocuments <$> decodeJson json
