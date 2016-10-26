module Hunt.CLI.Types
  ( -- Types
    CliCommand (..)
  , CliErr (..)
  , ServerOptions
  , Limit
  , Offset

    -- Helpers
  , defaultServerOptions
  , formatErr
  ) where


import           Data.Aeson
import qualified Data.Text      as T
import           GHC.Generics
import           Hunt.API       (Limit, Offset)
import           Servant.Client (BaseUrl (..), Scheme (Http), ServantError (..))


-- COMMAND

type ServerOptions = BaseUrl

-- | Expose default options for a connection with
-- the server. The default url is @http://localhost:3000@
defaultServerOptions :: ServerOptions
defaultServerOptions = BaseUrl
  { baseUrlScheme = Http
  , baseUrlHost   = "localhost"
  , baseUrlPort   = 3000
  , baseUrlPath   = ""
  }


-- | The Command enumerates every possible command,
-- which might be executed with this command line program.
data CliCommand
  = Eval ServerOptions FilePath
  | Search ServerOptions (Maybe Offset) (Maybe Limit) T.Text
  | Completion ServerOptions T.Text
  | MakeSchema FilePath
  | MakeInsert FilePath
  | FromCSV FilePath
  deriving (Show)


data CliErr
  = JsonErr String
  | HttpErr ServantError
  deriving (Show)


formatErr :: CliErr -> String
formatErr (JsonErr err) = "JSON parsing failed with err:\n" ++ err
formatErr (HttpErr err) =
  case err of
    FailureResponse status contentType body ->
      "Request failed with code " ++ show status ++ " and response body\n\n> " ++ show body

    DecodeFailure err _ body ->
      "Decoding response failed with error\n" ++ show err ++ "\n\non body\n\n>" ++
      show body

    ConnectionError err ->
      "Are you sure the Hunt server is running under the given URL? Here is the full error\n\n>" ++
      show err

    _ ->
      "Sorry, either the content type is not supported or invalid"

