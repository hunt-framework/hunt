module Hunt.CLI
  (
  ) where

import qualified Data.Text           as T
import           Options.Applicative
import           Servant.Client      (BaseUrl)


-- COMMAND

type ServerOptions = Maybe BaseUrl
type Offset = Int
type Limit  = Int

-- |
data Command
  = Eval FilePath ServerOptions
  | Load FilePath ServerOptions
  | Store FilePath ServerOptions
  | Search T.Text (Maybe Offset) (Maybe Limit) ServerOptions
  | Completion T.Text ServerOptions
  | MakeSchema FilePath
  | MakeInsert FilePath
  | FromCSV FilePath

