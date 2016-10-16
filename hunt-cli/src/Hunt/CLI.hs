module Hunt.CLI
  ( P.huntCLI
  , runCommand
  ) where

import           Control.Monad.Except
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Either                (either)
import qualified Data.Text                  as T
import qualified Hunt.CLI.Parser            as P
import           Hunt.CLI.Types
import qualified Hunt.Client                as HC
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
runCommand (Search opts offset limit query) = do
  result <- runExceptT $ search opts offset limit query
  putStrLn $ show result


eval :: ServerOptions -> FilePath -> Cmd I.CmdResult
eval opts path = readCmd >>= decodeJson >>= request opts . HC.eval
  where readCmd = lift $ LBS.readFile path


search :: ServerOptions -> Maybe Offset -> Maybe Limit -> T.Text -> Cmd (LimitedResult RankedDoc)
search opts offset limit query =
  request opts $ HC.search query offset limit


-- HELPERS

decodeJson :: FromJSON a => LBS.ByteString -> Cmd a
decodeJson = either throw return . eitherDecode
  where throw = throwError . JsonErr


request :: ServerOptions -> HC.HuntClient a -> Cmd a
request opts req = do
  manager <- lift $ newManager defaultManagerSettings
  withExceptT HttpErr $ req manager opts

