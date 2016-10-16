module Hunt.CLI.Types
  ( CliCommand (..)
  , CliErr (..)
  , ServerOptions
  , Limit
  , Offset
  ) where


import qualified Data.Text      as T
import           Hunt.API       (Limit, Offset)
import           Servant.Client (BaseUrl, ServantError)


-- COMMAND

type ServerOptions = BaseUrl


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
