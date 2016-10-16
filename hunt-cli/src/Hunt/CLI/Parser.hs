module Hunt.CLI.Parser
  ( huntCLI
  ) where


import qualified Data.Text           as T
import           Hunt.CLI.Types
import           Options.Applicative
import           Servant.Client      (BaseUrl, parseBaseUrl)


-- API

-- | Build the full parser for parsing a @Command@
-- from command line arguments.
huntCLI :: ParserInfo CliCommand
huntCLI  = info (helper <*> commands)
    ( fullDesc
    <> progDesc "Query the server or work with a schema."
    <> header "A command line interface for the Hunt server."
    )


-- COMMANDS

commands :: Parser CliCommand
commands =
  subparser
  (  cmd "eval"        eval       "Evaluate command in a given file on the Hunt server"
  <> cmd "load"        load       "Load an index from a given file"
  <> cmd "store"       store      "Store an index to a given file"
  <> cmd "search"      search     "Search the Hunt server for a given query" 
  <> cmd "complete"    complete   "Retrieve completion proposals for a given query"
  <> cmd "make-schema" makeSchema "Print JSON schema for a document"
  <> cmd "make-insert" makeInsert "Print JSON command for insertion of document"
  <> cmd "from-csv"    fromCsv    "Convert CSV to JSON and print the result" )
  where
    cmd name parser desc = 
      command name (info (helper <*> parser) (progDesc desc))


eval :: Parser CliCommand
eval = Eval <$> serverOptions <*> file


load :: Parser CliCommand
load = Load <$> serverOptions <*> file


store :: Parser CliCommand
store = Store <$> serverOptions <*> file


search :: Parser CliCommand
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


complete :: Parser CliCommand
complete = Completion <$> serverOptions <*> query
  where
    query = T.pack <$> (argument str (metavar "QUERY"))


makeSchema :: Parser CliCommand
makeSchema = MakeSchema <$> file


makeInsert :: Parser CliCommand
makeInsert = MakeInsert <$> file


fromCsv :: Parser CliCommand
fromCsv = FromCSV <$> file


-- HELPER PARSERS

file :: Parser FilePath
file = argument str
  ( metavar "FILE"
   <> help "File to read command input from" )


serverOptions :: Parser ServerOptions
serverOptions = parseUrl <$> baseUrl
  where
    baseUrl =
      strOption
      ( long "baseUrl"
      <> short 's'
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
