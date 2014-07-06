{-# LANGUAGE DoAndIfThenElse #-}
-- ----------------------------------------------------------------------------
{- |
  Main module for the executable.
-}
-- ----------------------------------------------------------------------------

module Main where

import           Control.Applicative       ((<$>), (<|>))
import           Control.Monad             (when)

import           Data.Aeson                (FromJSON, decode, encode)
import           Data.Aeson.Encode.Pretty  (encodePretty)
import           Data.ByteString.Lazy      (ByteString)
import           Data.Char                 (toUpper)
import           Data.Map                  (keys)
import           Data.Maybe                (fromJust)
import           Data.String.Conversions   (cs)
import           Data.Text                 (Text)
import           Data.Time.Clock.POSIX     (getPOSIXTime)

import           System.Console.Docopt     (argument, command, getArg,
                                            isPresent, longOption,
                                            optionsWithUsage)
import           System.Environment        (getArgs)
import qualified System.IO                 as System (stderr)
import qualified System.Log.Formatter      as Log (simpleLogFormatter)
import qualified System.Log.Handler        as Log (setFormatter)
import qualified System.Log.Handler.Simple as Log (streamHandler)
import qualified System.Log.Logger         as Log

import qualified Hunt.ClientInterface      as H
import qualified Hunt.Common.ApiDocument   as H
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
    , "  hunt-server-cli search <query>"
    , "  hunt-server-cli completion <query>"
    , "  hunt-server-cli make-schema <file>"
    , "  hunt-server-cli make-insert <file>"
    , "  hunt-server-cli from-csv <file>"
    , ""
    , ""
    , "Options:"
    , "  -h --help           Show this screen."
    , "  --server=SERVER     Use this hunt server [default: http://localhost:3000]"
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


readDocuments :: ByteString -> [H.ApiDocument]
readDocuments bs = maybe [] id ((return <$> decode bs) <|> decode bs)

makeSchema :: FilePath -> IO String
makeSchema fileName = do
  file <- cs <$> readFile fileName
  let docs = readDocuments file
      names = keys $ H.adIndex $ head docs
      cmds = (\name -> H.cmdInsertContext name H.mkSchema) <$> names
  return $ cs $ encodePretty cmds

makeInserts :: FilePath -> IO String
makeInserts fileName = do
  file <- cs <$> readFile fileName
  let docs = readDocuments file
  return $ cs $ encodePretty $ H.cmdSequence $ H.cmdInsertDoc <$> docs

evalCmd :: String -> H.Command -> IO ByteString
evalCmd server cmd = do
  HC.withHuntServer (HC.eval cmd) (cs server)

eval :: String -> FilePath -> IO ByteString
eval server fileName = do
  file <- readFile fileName
  evalCmd server $ fromJust $ decode $ cs file

search :: String -> String -> IO ByteString
search server query
    = cs
      <$> (encodePretty :: H.LimitedResult H.ApiDocument -> ByteString)
      <$> (HC.withHuntServer (HC.query (cs query) 0 ) (cs server))

autocomplete :: String -> String -> IO String
autocomplete server query
    = cs
      <$> (encodePretty :: [Text] -> ByteString)
      <$> (HC.withHuntServer (HC.autocomplete $ cs query) (cs server))

-- | Main function for the executable.
main :: IO ()
main = do
  initLoggers Log.DEBUG
  args <- optionsWithUsage usage =<< getArgs

  let isCommand str = args `isPresent` (command str)
      fileArgument = args `getArg` (argument "<file>")
      queryArgument = args `getArg` (argument "<query>")

  server <- do
      if (args `isPresent` (longOption "server")) then
          args `getArg` (longOption "server")
      else
          return "http://localhost:3000"

  when (isCommand "eval") $ do
    file <- fileArgument
    putStr =<< (printTime $ cs <$> eval server file)

  when (isCommand "load") $ do
    file <- fileArgument
    putStr =<< cs <$> (evalCmd server $ H.cmdLoadIndex file)

  when (isCommand "store") $ do
    file <- fileArgument
    putStr =<< cs <$> (evalCmd server $ H.cmdStoreIndex file)

  when (isCommand "search") $ do
    query <- queryArgument
    putStr =<< (printTime $ cs <$> search server query)

  when (isCommand "completion") $ do
    query <- queryArgument
    putStr =<< (printTime $ cs <$> autocomplete server query)

  when (isCommand "from-csv") $ do
    CSV.convert =<< fileArgument

  when (isCommand "make-schema") $ do
    file <- fileArgument
    putStr =<< makeSchema file

  when (isCommand "make-insert") $ do
    file <- fileArgument
    putStr =<< makeInserts file

