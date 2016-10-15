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
huntCLI :: ParserInfo Command
huntCLI  = info (helper <*> commands)
    ( fullDesc
    <> progDesc "Query the server or work with a schema."
    <> header "A command line interface for the Hunt server."
    )


-- COMMANDS

commands :: Parser Command
commands =
  subparser
  (  command "eval" (info (helper <*> eval)
      ( progDesc "Evaluate command in a given file on the Hunt server" ))
  <> command "load" (info (helper <*> load)
      ( progDesc "Load an index from a given file" ))
  <> command "store" (info (helper <*> store)
      ( progDesc "Store an index to a given file" ))
  <> command "search" (info (helper <*> search)
      ( progDesc "Search the Hunt server for a given query" ))
  <> command "complete" (info (helper <*> complete)
      ( progDesc "Retrieve completion proposals for a given query" ))
  <> command "make-schema" (info (helper <*> makeSchema)
      ( progDesc "Print JSON schema for a document" ))
  <> command "make-insert" (info (helper <*> makeInsert)
      ( progDesc "Print JSON command for insertion of document" ))
  <> command "from-csv" (info (helper <*> fromCsv)
      ( progDesc "Convert CSV to JSON and print the result" ))
  )


eval :: Parser Command
eval = Eval <$> serverOptions <*> file


load :: Parser Command
load = Load <$> serverOptions <*> file


store :: Parser Command
store = Store <$> serverOptions <*> file


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
