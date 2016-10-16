module Hunt.CLI
  ( P.huntCLI
  , runCommand
  ) where

import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Either                (either)
import qualified Data.Text                  as T
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
import           Servant.Client             (BaseUrl)


-- EXECUTING COMMANDS

-- | A Cmd either entails an CliErr or
-- some result to be executed in the IO monad
type Cmd = ExceptT CliErr IO


runCommand :: CliCommand -> IO ()
runCommand command =
  case command of
    Eval opts file ->
      printCmd (putStrLn . show) $ eval opts file

    Load opts file ->
      printCmd printSuccess $ load opts file
      where printSuccess = const $ putStrLn $ "Successfully loaded index from " ++ file

    Store opts file ->
      printCmd printSuccess $ store opts file
      where printSuccess = const $ putStrLn $ "Successfully stored index in " ++ file

    Search opts offset limit query ->
      printCmd printJson $ search opts offset limit query

    Completion opts query ->
      printCmd printJson $ complete opts query

    MakeSchema file ->
      printCmd printJson $ makeSchema file

    MakeInsert file ->
      printCmd printJson $ makeInsert file

    FromCSV file ->
      putStrLn "Not implemented yet!"


-- OPERATIONS

eval :: ServerOptions -> FilePath -> Cmd I.CmdResult
eval opts file = readAll file >>= decodeJson >>= request opts . HC.eval


load :: ServerOptions -> FilePath -> Cmd ()
load opts fileName = request opts $ HC.loadIndex $ T.pack fileName


store :: ServerOptions -> FilePath -> Cmd ()
store opts fileName = request opts $ HC.storeIndex $ T.pack fileName


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


request :: ServerOptions -> HC.HuntClient a -> Cmd a
request opts req = do
  manager <- lift $ newManager defaultManagerSettings
  withExceptT HttpErr $ req manager opts


decodeApiDocs :: LBS.ByteString -> Cmd [HC.ApiDocument]
decodeApiDocs json =
  HC.insertCmdsToDocuments <$> decodeJson json
