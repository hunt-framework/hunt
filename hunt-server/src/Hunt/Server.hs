{-# LANGUAGE OverloadedStrings #-}
-- http://ghc.haskell.org/trac/ghc/blog/LetGeneralisationInGhc7
-- {-# LANGUAGE NoMonoLocalBinds  #-}

module Hunt.Server {-(start)-} where

import           Control.Monad.Error

import           Data.Text                            (Text)

import           Network.Wai.Middleware.RequestLogger

import           Hunt.Interpreter.Command
import           Hunt.Interpreter.Interpreter

import           Hunt.Query.Language.Parser
import           Hunt.Query.Ranking

import           Hunt.Server.Common
import           Hunt.Server.Schrotty             hiding (Options)
import qualified Hunt.Server.Template             as Tmpl

import           System.IO                            (stdout)

import           System.Log.Formatter
import           System.Log.Handler
import           System.Log.Handler.Simple
import           System.Log.Logger                    hiding (debugM, warningM, errorM)
import qualified System.Log.Logger                    as Log

-- ----------------------------------------------------------------------------
-- Application launch options

data Options = Options
  { optLogLevel :: Priority
  }

defaultOptions :: Options
defaultOptions = Options
  { optLogLevel = DEBUG
  }

-- ----------------------------------------------------------------------------
-- Logging

-- | Name of the module for logging purposes.
modName :: String
modName = "Hunt.Server"

-- | Location of the log file. 
logPath :: String
logPath = "hunt.log"

-- | Log a message at 'DEBUG' priority.
debugM :: String -> IO ()
debugM = Log.debugM modName

-- | Log a message at 'WARNING' priority.
warningM :: String -> IO ()
warningM = Log.warningM modName

-- | Log a message at 'ERROR' priority.
errorM :: String -> IO ()
errorM = Log.errorM modName

-- | Convenience function to add a log formatter.
withFormatter :: (Monad m, LogHandler r) => m r -> LogFormatter r -> m r
withFormatter h f = liftM (flip setFormatter f) h

-- | Initializes the loggers and sets the stdout logger to the given priority.
initLoggers :: Priority -> IO ()
initLoggers level = do
    -- formatter
    let defFormatter = simpleLogFormatter "[$time : $loggername : $prio] $msg"

    -- root does not have a priority
    updateGlobalLogger rootLoggerName clearLevel

    -- stdout root logger
    handlerBare <- streamHandler stdout level `withFormatter` defFormatter
    updateGlobalLogger rootLoggerName (setHandlers [handlerBare])

    -- file logger always at 'DEBUG' level
    handlerFile <- fileHandler logPath DEBUG `withFormatter` defFormatter
    updateGlobalLogger rootLoggerName (addHandler handlerFile)

-- ----------------------------------------------------------------------------

start :: IO ()
start = do
  -- initialize loggers
  initLoggers $ optLogLevel defaultOptions

  debugM "Application start"

  -- init interpreter
  env <- initEnv emptyIndexer defaultRankConfig contextTypes

  -- start schrotty
  schrotty 3000 $ do

    -- request / response logging
    middleware logStdoutDev

    -- ------------------------------------------------------------------------
    -- XXX: maybe move to schrotty?

    let interpret = runCmd env

    let eval cmd = do
        res <- liftIO $ interpret cmd
        case res of
          Left res' ->
            throw $ InterpreterError res'
          Right res' ->
            case res' of
              ResOK               -> json $ JsonSuccess ("ok" :: Text)
              ResSearch docs      -> json $ JsonSuccess docs
              ResCompletion wrds  -> json $ JsonSuccess wrds
              ResGeneric val      -> json $ JsonSuccess val

    let evalQuery mkCmd q = case parseQuery q of
          Right qry -> eval $ mkCmd qry
          Left  err -> throw $ Json 700 err

    let batch cmd = Sequence . map cmd

    -- ------------------------------------------------------------------------

    get "/"         $ redirect "/search"
    get "/search"   $ html . Tmpl.index $ (0::Int)
    get "/add"      $ html Tmpl.addDocs

    -- ------------------------------------------------------------------------

    -- interpreter
    post "/eval" $ do
      cmd <- jsonData
      eval cmd

    -- ------------------------------------------------------------------------

    -- simple query
    get "/search/:query/" $ do
      query    <- param "query"
      evalQuery (\q -> Search q 0 1000000) query

    -- paged query
    get "/search/:query/:offset/:mx" $ do
      query    <- param "query"
      offset   <- param "offset"
      mx       <- param "mx"
      evalQuery (\q -> Search q offset mx) query

    -- completion
    get "/completion/:query/:mx" $ do
      query <- param "query"
      mx    <- param "mx"
      evalQuery (\q -> Completion q mx) query

    -- insert a document (fails if a document (the uri) already exists)
    post "/document/insert" $ do
      jss <- jsonData
      eval $ batch Insert jss

    -- update a document (fails if a document (the uri) does not exist)
    post "/document/update" $ do
      jss <- jsonData
      eval $ batch Update jss

    -- delete a set of documents by URI
    post "/document/delete" $ do
      jss <- jsonData
      eval $ batch Delete jss

    -- write the indexer to disk
    get "/binary/save/:filename" $ do
      filename  <- param "filename"
      eval $ StoreIx filename

    -- load indexer from disk
    get "/binary/load/:filename" $ do
      filename  <- param "filename"
      eval $ LoadIx filename

    -- status commands
    get "/status/gc" $ do
      eval $ Status StatusGC            -- garbage collector status

    get "/status/index" $ do
      eval $ Status StatusIndex         -- status of search index

    notFound $ throw NotFound
