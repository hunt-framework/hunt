{-# LANGUAGE DoAndIfThenElse #-}
-- ----------------------------------------------------------------------------
{- |
  Main module for the executable.
-}
-- ----------------------------------------------------------------------------

module Main where

import           Control.Applicative       ((<$>), (<|>))
import           Control.Monad             (when)

import           Data.Aeson                (decode)
import           Data.Aeson.Encode.Pretty  (encodePretty)
import           Data.ByteString.Lazy      (ByteString)
-- import           Data.Char                 (toUpper)
-- import           Data.Map                  (keys)
import           Data.Maybe                (fromJust)
import           Data.String.Conversions   (cs)
import           Data.Text                 (Text)
import           Data.Time.Clock.POSIX     (getPOSIXTime)

import           System.Console.Docopt.NoTH hiding (usage)

import           System.Environment        (getArgs)
import qualified System.IO                 as System (stderr)
import qualified System.Log.Formatter      as Log (simpleLogFormatter)
import qualified System.Log.Handler        as Log (setFormatter)
import qualified System.Log.Handler.Simple as Log (streamHandler)
import qualified System.Log.Logger         as Log

import qualified Hunt.ClientInterface      as H
import qualified Hunt.Converter.CSV        as CSV (convert)
import qualified Hunt.Server.Client        as HC

usage :: String
usage = unlines [
    "hunt-server-cli"
    , ""
    , "Usage:"
    , "  hunt-server-cli eval [--server SERVER] <file>"
    , "  hunt-server-cli load [--server SERVER] <file>"
    , "  hunt-server-cli store [--server SERVER] <file>"
    , "  hunt-server-cli search [--server SERVER] [--max MAX] [--offset OFFSET] <query>"
    , "  hunt-server-cli completion [--server SERVER] <query>"
    , "  hunt-server-cli make-schema <file>"
    , "  hunt-server-cli make-insert <file>"
    , "  hunt-server-cli from-csv <file>"
    , ""
    , ""
    , "Options:"
    , "  -h --help           Show this screen."
    , "  --server=SERVER     Use this hunt server [default: http://localhost:3000]"
    , "  --max=MAX           Request at most MAX results [default: 20]"
    , "  --offset=OFFSET     Request results from offset OFFSET [default: 0]"
    , "  make-schema <file>  prints a simple schema for this document" ]

-- ------------------------------------------------------------

-- | Initializes the loggers with the given priority.
initLoggers :: Log.Priority -> IO ()
initLoggers level = do
    handlerBare <- Log.streamHandler System.stderr Log.DEBUG
    let handler = Log.setFormatter handlerBare $ Log.simpleLogFormatter "[$time : $loggername : $prio] $msg"

    Log.updateGlobalLogger "" (Log.setLevel level . Log.setHandlers [handler])
    rl <- Log.getRootLogger
    Log.saveGlobalLogger rl

data Options = Options
  { optLogLevel ::Log.Priority
  }

printTime :: IO a -> IO a
printTime act = do

  start <- getTime
  result <- act
  end <- getTime
  let delta = end - start
  Log.infoM "main" $ "took " ++ (show (delta))
  return result
  where
  getTime = getPOSIXTime -- realToFrac `fmap`


readDocuments :: ByteString -> IO [H.ApiDocument]
readDocuments bs = maybe (do {(Log.infoM "main" $  "unable to parse ApiDocuments") ; return []}) return docs
  where
  docs = ((return <$> decode bs) <|> decode bs <|> (H.insertCmdsToDocuments <$> decode bs))

makeSchema :: FilePath -> IO String
makeSchema fileName = do
  file <- cs <$> readFile fileName
  docs <- readDocuments file
  let cmds = H.createContextCommands docs
  return $ cs $ encodePretty cmds

makeInserts :: FilePath -> IO String
makeInserts fileName = do
  file <- cs <$> readFile fileName
  docs <- readDocuments file
  return $ cs $ encodePretty $ H.cmdSequence $ H.cmdInsertDoc <$> docs

evalCmd :: String -> H.Command -> IO ByteString
evalCmd server cmd = do
  encodePretty <$> HC.evalOnServer_ (cs server) cmd

eval :: String -> FilePath -> IO ByteString
eval server fileName = do
  file <- readFile fileName
  evalCmd server $ fromJust $ decode $ cs file

search :: String -> String -> Int -> Int -> IO ByteString
search server query maxResults offsetResults
    = cs
      <$> (encodePretty :: H.LimitedResult H.ApiDocument -> ByteString)
      <$> HC.withHuntServer (cs server) (HC.getQuery (cs query) maxResults offsetResults )

autocomplete :: String -> String -> IO String
autocomplete server query
    = cs
      <$> (encodePretty :: [Text] -> ByteString)
      <$> (HC.withHuntServer (cs server) (HC.getAutocomplete $ cs query))

-- | Main function for the executable.
main :: IO ()
main = do
  initLoggers Log.DEBUG
  docopt <- parseUsageOrExit usage
  args   <- parseArgsOrExit docopt =<< getArgs

  let isCommand str = args `isPresent` (command str)
      fileArgument = getArgWithDefault args "" (argument "file")
      queryArgument = getArgWithDefault args "" (argument "query")
      file = fileArgument
      query = queryArgument

  let server     = getArgWithDefault args "http://localhost:3000" (longOption "server") 
      maxResults = read (getArgWithDefault args "0" (longOption "max"))
      offsetResults = read (getArgWithDefault args "0" (longOption "offset"))

  when (isCommand "eval") $ do
    putStr =<< (printTime $ cs <$> eval server file)

  when (isCommand "load") $ do
     putStr =<< cs <$> (evalCmd server $ H.cmdLoadIndex file)

  when (isCommand "store") $ do
     putStr =<< cs <$> (evalCmd server $ H.cmdStoreIndex file)

  when (isCommand "search") $ do
     putStr =<< (printTime $ cs <$> search server query maxResults offsetResults)

  when (isCommand "completion") $ do
     putStr =<< (printTime $ cs <$> autocomplete server query)

  when (isCommand "from-csv") $ do
    CSV.convert fileArgument

  when (isCommand "make-schema") $ do
    putStr =<< makeSchema file

  when (isCommand "make-insert") $ do
    putStr =<< makeInserts file

