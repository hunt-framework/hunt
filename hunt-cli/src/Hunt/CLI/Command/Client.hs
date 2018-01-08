module Hunt.CLI.Command.Client
  ( Command (..)
  , Error (..)
  , run
  , parser
  ) where


import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Csv                   as CSV
import           Data.Either                (either)
import qualified Data.Map                   as M
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import qualified Hunt.API                   as HC
import qualified Hunt.Client                as HC
import qualified Hunt.ClientInterface       as HC
import           Hunt.Common.ApiDocument    (LimitedResult)
import           Hunt.Interpreter
import qualified Hunt.Interpreter.Command   as I
import           Hunt.Query.Intermediate    (RankedDoc)
import qualified Hunt.Server                as Server
import           Network.HTTP.Client
import           Options.Applicative
import           Servant.Client             (BaseUrl, ClientM)
import qualified Servant.Client             as Servant



-- CLIENT COMMANDS


data Command
  = Eval Servant.BaseUrl FilePath
  | Search Servant.BaseUrl (Maybe HC.Offset) (Maybe HC.Limit) T.Text
  | Completion Servant.BaseUrl T.Text
  | MakeSchema FilePath
  | MakeInsert FilePath
  | FromCSV FilePath
  deriving (Show)



-- ERROR


data Error
  = JsonError String
  | HttpError Servant.ServantError
  deriving (Show)


formatError :: Error -> String
formatError (JsonError err) = "JSON parsing failed with err:\n" ++ err
formatError (HttpError err) =
  case err of
    Servant.FailureResponse (Servant.Response status body headers version) ->
      "Request failed with code " ++ show status ++ " and response body\n\n> " ++ show body

    Servant.DecodeFailure err (Servant.Response status body headers version) ->
      "Decoding response failed with error\n" ++ show err ++ "\n\non body\n\n>" ++
      show body

    Servant.ConnectionError err ->
      "Are you sure the Hunt server is running under the given URL? Here is the full error\n\n>" ++
      show err

    _ ->
      "Sorry, either the content type is not supported or invalid"




-- PARSE FROM COMMAND LINE


parser :: Parser Command
parser = subparser
  (  cmd "eval"        eval       "Evaluate command in a given file on the Hunt server"
  <> cmd "search"      search     "Search the Hunt server for a given query"
  <> cmd "complete"    complete   "Retrieve completion proposals for a given query"
  <> cmd "make-schema" makeSchema "Print JSON schema for a document"
  <> cmd "make-insert" makeInsert "Print JSON command for insertion of document"
  <> cmd "from-csv"    fromCsv    "Convert CSV to JSON and print the result"
  )
  where
    cmd name parser desc =
      command name (info (helper <*> parser) (progDesc desc))

    eval :: Parser Command
    eval = Eval <$> serverOptions <*> file

    search :: Parser Command
    search = Search <$> serverOptions <*> offset <*> limit <*> query
      where
        query = T.pack <$> (argument str (metavar "QUERY"))

        offset = optional $
          option auto
          ( long "offset"
            <> help "Offset from which to start listing results" )

        limit = optional $
          option auto
          ( long "limit"
            <> help "Maximum number of results" )

    complete :: Parser Command
    complete = Completion <$> serverOptions <*> query
      where
        query = T.pack <$> (argument str (metavar "QUERY"))

    makeSchema :: Parser Command
    makeSchema = MakeSchema <$> file

    makeInsert :: Parser Command
    makeInsert = MakeInsert <$> file

    fromCsv :: Parser Command
    fromCsv = FromCSV <$> file



-- HELPER PARSERS


file :: Parser FilePath
file = argument str
  ( metavar "FILE"
   <> help "File to read command input from" )


serverOptions :: Parser Servant.BaseUrl
serverOptions =
  option parseUrl
  ( long "baseUrl"
  <> short 's'
  <> value defaultBaseUrl
  <> (help $ "Base URL of the Hunt server. Defaults to " ++ show defaultBaseUrl ))
  where
    parseUrl = eitherReader $
      either (Left . show) Right . Servant.parseBaseUrl

    defaultBaseUrl :: BaseUrl
    defaultBaseUrl = Servant.BaseUrl
      { Servant.baseUrlScheme = Servant.Http
      , Servant.baseUrlHost   = "localhost"
      , Servant.baseUrlPort   = 3000
      , Servant.baseUrlPath   = ""
      }



-- RUN


type Cmd
  = ExceptT Error IO


run :: Command -> IO ()
run command =
  case command of
    Eval opts file ->
      printCmd (putStrLn . show) $
        readAll file >>= decodeJson >>= request opts . HC.eval

    Search opts offset limit query ->
      printCmd printJson $
        (request opts (HC.searchText query offset limit) :: ExceptT Error IO (HC.LimitedResult RankedDoc))

    Completion opts query ->
      printCmd printJson $
        request opts $ HC.completeText query Nothing

    MakeSchema file ->
      printCmd printJson $
        readAll file >>= decodeApiDocs >>= pure . HC.createContextCommands

    MakeInsert file ->
      printCmd printJson $
        readAll file >>= decodeApiDocs >>= pure . HC.cmdSequence . fmap HC.cmdInsertDoc

    FromCSV file ->
      let
        eval input =
          undefined
      in
        error "This operation is not supported yet."



-- HELPERS


printCmd :: (a -> IO ()) -> Cmd a -> IO ()
printCmd f cmd = runExceptT cmd >>= either printErr f
  where printErr = putStrLn . formatError


printJson :: ToJSON a => a -> IO ()
printJson =
  LBS.putStrLn . encodePretty


readAll :: FilePath -> Cmd LBS.ByteString
readAll =
  lift . LBS.readFile


decodeJson :: FromJSON a => LBS.ByteString -> Cmd a
decodeJson =
  either (throwError . JsonError) return . eitherDecode


request :: Servant.BaseUrl -> ClientM a -> Cmd a
request baseUrl req = do
  client <- lift $ HC.withBaseUrl baseUrl
  result <- lift $ HC.runClientM req client
  either (throwError . HttpError) return result


decodeApiDocs :: LBS.ByteString -> Cmd [HC.ApiDocument]
decodeApiDocs json =
  HC.insertCmdsToDocuments <$> decodeJson json
