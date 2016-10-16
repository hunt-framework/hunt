module Hunt.CLI.Types
  ( -- Types
    CliCommand (..)
  , CliErr (..)
  , ServerOptions
  , Limit
  , Offset

    -- Helpers
  , defaultServerOptions
  ) where


import qualified Data.Text      as T
import           Hunt.API       (Limit, Offset)
import           Servant.Client (BaseUrl (..), Scheme (Http), ServantError)


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
  | Load ServerOptions FilePath
  | Store ServerOptions FilePath
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
