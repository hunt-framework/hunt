module Hunt.CLI
  (
  ) where

import qualified Data.Text           as T
import           Options.Applicative
import           Servant.Client      (BaseUrl, parseBaseUrl)


-- COMMAND

type ServerOptions = BaseUrl
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


-- PARSERS

serverOptions :: Parser ServerOptions
serverOptions = parseUrl <$> baseUrl
  where
    baseUrl =
      strOption
      ( long "baseUrl"
      <> short 's'
      <> metavar "BASE_URL"
      <> value "http://localhost:3000"
      <> help "Base URL of the Hunt server. Defaults to http://localhost:3000" )
          
    parseUrl url =
      case parseBaseUrl url of
        Right baseUrl ->
          baseUrl

        Left err ->
          -- It would be much nicer to be able to throw an
          -- error with optparse-applicative. While partial functions
          -- generally should be avoided, since this is a cli and
          -- an error in the BaseUrl would stop any request from
          -- happening anyways, this seems to be ok. Though hacky, still.
          error $ show err
        


