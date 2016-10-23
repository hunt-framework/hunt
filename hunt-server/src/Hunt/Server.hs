{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- http://ghc.haskell.org/trac/ghc/blog/LetGeneralisationInGhc7
-- {-# LANGUAGE NoMonoLocalBinds  #-}

-- ----------------------------------------------------------------------------
{- |
  The Hunt server.

  Routes:

    [@POST \/eval@]                          Evaluates 'Command's.

    [@GET  \/search\/:query\/@]              Search (unlimited # of results).

    [@GET  \/search\/:query\/:offset\/:mx@]  Search with pagination.

    [@GET  \/weight\/:query\/@]              Search and return weights of documents

    [@GET  \/completion\/:query\/:mx@]       Word completions with maximum.

    [@POST \/document\/insert@]              Insert 'ApiDocument's.

    [@POST \/document\/update@]              Update 'ApiDocument's.

    [@POST \/document\/delete@]              Delete documents by URI.

    [@GET  \/binary\/save\/:filename@]       Store the index.

    [@GET  \/binary\/load\/:filename@]       Load an index.

    [@GET  \/status\/gc@]                    Garbage collection statistics.

    [@GET  \/status\/doctable@]              JSON dump of the document table (/experimental/).

    [@GET  \/status\/index@]                 JSON dump of the index (/experimental/).
-}
-- ----------------------------------------------------------------------------

module Hunt.Server
       ( -- * Starting the Server
         start
         -- * Configuration
       , HuntServerConfiguration (..)
       )
where

import           Control.Monad.Except
import           Data.String (fromString)
import           Data.Text (Text)
import           Hunt.ClientInterface
import           Hunt.Interpreter
import           Hunt.Interpreter.Command (StatusCmd (..))
import           Hunt.Server.Common
import qualified Hunt.Server.Schrotty as Schrotty
import           Hunt.Server.Schrotty hiding (Options)
import qualified Hunt.Server.Template as Tmpl
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Middleware.RequestLogger
import           System.IO (stdout)
import           System.Log.Formatter
import           System.Log.Handler
import           System.Log.Handler.Simple
import qualified System.Log.Logger as Log
import           System.Log.Logger hiding (debugM, errorM, warningM)

#ifdef SUPPORT_STATSD
import qualified Data.Text as T
import           System.Remote.Monitoring.Statsd (defaultStatsdOptions, forkStatsd, StatsdOptions(..))
import           System.Metrics (registerGcMetrics, newStore)
#endif

-- ------------------------------------------------------------
-- Logging

-- | Name of the module for logging purposes.
modName :: String
modName = "Hunt.Server"

-- | Log a message at 'DEBUG' priority.
debugM :: String -> IO ()
debugM = Log.debugM modName
{-
-- | Log a message at 'WARNING' priority.
warningM :: String -> IO ()
warningM = Log.warningM modName

-- | Log a message at 'ERROR' priority.
errorM :: String -> IO ()
errorM = Log.errorM modName
-}

-- | Convenience function to add a log formatter.
withFormatter :: (Monad m, LogHandler r) => m r -> LogFormatter r -> m r
withFormatter h f = liftM (flip setFormatter f) h

-- | Initializes the loggers (stdout, file).
--   Sets the stdout logger to the given priority
--   and sets the path and 'DEBUG' priority for the file logger.
initLoggers :: HuntServerConfiguration -> IO ()
initLoggers config = do
  -- formatter
  let defFormatter = simpleLogFormatter "[$time : $loggername : $prio] $msg"

  -- root does not have a priority
  updateGlobalLogger rootLoggerName clearLevel

  -- stdout root logger
  handlerBare <- streamHandler stdout (logPriority config) `withFormatter` defFormatter
  updateGlobalLogger rootLoggerName (setHandlers [handlerBare])

  -- file logger always at 'DEBUG' level
  handlerFile <- fileHandler (logFile config) DEBUG `withFormatter` defFormatter
  updateGlobalLogger rootLoggerName (addHandler handlerFile)

-- ------------------------------------------------------------

-- | Start the server.
start :: HuntServerConfiguration -> IO ()
start config = do
  -- initialize loggers
  initLoggers config

  debugM "Application start"

#ifdef SUPPORT_STATSD
  when (not $ null $ statsDHost config) $ do
    statsDStore <- newStore
    registerGcMetrics statsDStore
    let statsDOpts = defaultStatsdOptions {host = (T.pack $ statsDHost config), port = (statsDPort config), prefix = "Hunt-Server"}
    _ <- forkStatsd statsDOpts statsDStore
    debugM $ "Connected to statsD: " ++ (statsDHost config) ++ ":" ++ (show $ statsDPort config)
#endif

  -- init interpreter
  env <- initHunt "tmp-index" :: IO DefHuntEnv

  case readIndexOnStartup config of
    Just filename -> do
      res <- liftIO $ runCmd env $ cmdLoadIndex filename
      case res of
        Right _  -> debugM $ "Index loaded: " ++ filename
        Left err -> fail $ show err
    Nothing -> return ()

  let options1 = Schrotty.Options
        { verbose  = 1
        , settings = W.setHost (fromString $ huntServerHost config)
                   $ W.setPort (huntServerPort config)
                   $ W.defaultSettings
        }
  -- start schrotty
  schrottyOpts options1 $ do

    -- request / response logging
    middleware logStdoutDev

    -- ------------------------------------------------------------
    -- XXX: maybe move to schrotty?

    let interpret = runCmd env

    let eval cmd = do
        res <- liftIO $ interpret cmd
        case res of
          Left res' -> do
            raise $ InterpreterError res'
          Right res' ->
            case res' of
              ResOK               -> json $ JsonSuccess ("ok" :: Text)
              ResSearch docs      -> json $ JsonSuccess docs
              ResCompletion wrds  -> json $ JsonSuccess wrds
              ResSuggestion wrds  -> json $ JsonSuccess wrds
              ResGeneric val      -> json $ JsonSuccess val

    let evalQuery mkCmd q = case parseQuery q of
          Right qry -> eval $ mkCmd qry
          Left  err -> raise $ Json 700 err

    let batch cmd = cmdSequence . map cmd

    -- ------------------------------------------------------------

    get "/"           $ redirect "/search"
    get "/search"     $ html . Tmpl.index $ (0::Int)
    get "/quickstart" $ html Tmpl.quickstart

    -- ------------------------------------------------------------

    -- interpreter
    post "/eval" $ do
      cmd <- jsonData
      eval cmd

    -- ------------------------------------------------------------

    -- simple query with unlimited # of hits
    get "/search/:query/" $ do
      query    <- param "query"
      evalQuery cmdSearch query

    -- simple query with unlimited # of hits
    get "/select/:query/" $ do
      query    <- param "query"
      evalQuery cmdSelect query

    -- paged query
    get "/search/:query/:offset/:mx" $ do
      query    <- param "query"
      offset   <- param "offset"
      mx       <- param "mx"
      evalQuery ( setResultOffset offset
                  . setMaxResults mx
                  . cmdSearch
                ) query

    -- simple query for reading the weight of documents
    get "/weight/:query/" $ do
      query    <- param "query"
      evalQuery ( setWeightIncluded
                  . setSelectedFields []
                  . cmdSearch
                ) query

    -- completion
    get "/completion/:query/:mx" $ do
      query <- param "query"
      mx    <- param "mx"
      evalQuery ( setMaxResults mx
                  . cmdCompletion
                ) query

    -- simple query with unlimited # of hits
    get "/select/:query/" $ do
      query    <- param "query"
      evalQuery (\q -> cmdSelect q) query

    -- insert a document (fails if a document (the uri) already exists)
    post "/document/insert" $ do
      jss <- jsonData
      eval $ batch cmdInsertDoc jss

    -- update a document (fails if a document (the uri) does not exist)
    post "/document/update" $ do
      jss <- jsonData
      eval $ batch cmdUpdateDoc jss

    -- delete a set of documents by URI
    post "/document/delete" $ do
      jss <- jsonData
      eval $ batch cmdDeleteDoc jss

    -- write the indexer to disk
    get "/binary/save/:filename" $ do
      filename  <- param "filename"
      eval $ cmdStoreIndex filename

    -- load indexer from disk
    get "/binary/load/:filename" $ do
      filename  <- param "filename"
      eval $ cmdLoadIndex filename

    -- status commands
    get "/status/gc" $ do
      eval $ cmdStatus StatusGC            -- garbage collector status

    get "/status/doctable" $ do
      eval $ cmdStatus StatusDocTable      -- status of document table

    get "/status/index" $ do
      eval $ cmdStatus StatusIndex         -- status of search index

    get "/status/context/:cx" $ do
      query <- param "cx"
      eval $ cmdStatus (StatusContext query)

    notFound $ raise NotFound

-- ------------------------------------------------------------
