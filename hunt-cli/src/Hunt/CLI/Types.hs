module Hunt.CLI.Types
  ( Command (..)
  , ServerOptions
  , Limit
  , Offset
  ) where


import qualified Data.Text      as T
import           Hunt.API       (Limit, Offset)
import           Servant.Client (BaseUrl)


-- COMMAND

type ServerOptions = BaseUrl


-- | The Command enumerates every possible command,
-- which might be executed with this command line program.
data Command
  = Eval ServerOptions FilePath
  | Load ServerOptions FilePath
  | Store ServerOptions FilePath
  | Search ServerOptions (Maybe Offset) (Maybe Limit) T.Text
  | Completion ServerOptions T.Text
  | MakeSchema FilePath
  | MakeInsert FilePath
  | FromCSV FilePath
  deriving (Show)
