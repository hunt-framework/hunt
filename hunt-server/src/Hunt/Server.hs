{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Hunt.Server
  ( -- Configuration
    HuntServerConfiguration (..)
  , defaultConfig

    -- Server
  , serveApp
  , runWithConfig
  ) where


import qualified Data.ByteString.Lazy                 as LB
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as TE

import           Control.Monad.Except

import qualified Hunt.ClientInterface                 as HC
import           Hunt.Common.ApiDocument              (LimitedResult)
import           Hunt.Interpreter
import           Hunt.Interpreter.Command             (CmdResult (..),
                                                       Command (..),
                                                       StatusCmd (..), ceMsg)
import           Hunt.Query.Intermediate              (RankedDoc)

import           Network.Wai                          (Application)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

import           Hunt.Server.API
import           Hunt.Server.Configuration
import qualified Hunt.Server.Template                 as Templ
import           Servant

import           Servant.HTML.Blaze
import           System.Directory                     (getCurrentDirectory)
import           System.IO                            (stdout)
import           System.Log.Formatter
import           System.Log.Handler
import           System.Log.Handler.Simple
import           System.Log.Logger                    hiding (debugM, errorM,
                                                       warningM)
import qualified System.Log.Logger                    as Log
import           Text.Blaze.Html                      (Html)


-- SERVER

-- | Initialize DefHuntEnv based on the configuration
-- and start a server using the configuration specified.
runWithConfig :: HuntServerConfiguration -> IO ()
runWithConfig config = do
  indexDir <- getIndexDir $ indexDirectory config
  env <- initHunt indexDir :: IO DefHuntEnv
  initLoggers (logPriority config) (logFile config)
  run (huntServerPort config) (serveApp env)


getIndexDir :: FilePath -> IO FilePath
getIndexDir configuredDir
  | configuredDir == defaultDir = defaultDirectory
  | otherwise = return configuredDir
  where
    defaultDir = indexDirectory defaultConfig

    defaultDirectory = do
      dir <- getCurrentDirectory
      return $ dir ++ "/" ++ "data"


-- | Combine the HuntAPI with the server implementation
-- to serve an application.
serveApp :: DefHuntEnv -> Application
serveApp = logStdoutDev . serve huntServerAPI . server


server :: DefHuntEnv -> Server HuntServerAPI
server env = huntServer
        :<|> html
  where
    huntServer :: Server HuntAPI
    huntServer = search env
            :<|> completion env
            :<|> documents env
            :<|> evaluate env
            :<|> weight env
            :<|> select env
            :<|> status env


-- | Provide server implementation of the search API based
-- on the given Hunt Environment.
search :: DefHuntEnv -> Server SearchAPI
search env = search'
  where
    search' :: T.Text -> Maybe Offset -> Maybe Limit -> HuntResult (LimitedResult RankedDoc)
    search' query offset limit
      = evalQuery cmdSearch env query >>= getLimitedResult
      where
        withOffset = maybe id HC.setResultOffset offset
        withLimit  = maybe id HC.setMaxResults limit
        cmdSearch  = withOffset . withLimit . HC.cmdSearch


-- | Provide server implementation of the completion API based
-- on the given Hunt environment
completion :: DefHuntEnv -> Server CompletionAPI
completion env = complete
  where
    complete :: T.Text -> Maybe Limit -> HuntResult Suggestion
    complete query limit
      = evalQuery cmdComplete env query >>= getCompletionResult
      where
        withLimit = maybe id HC.setMaxResults limit
        cmdComplete = withLimit . HC.cmdCompletion


-- | Provide server implementation of the document API based
-- on the given Hunt environment
documents :: DefHuntEnv -> Server DocumentAPI
documents env = insert
           :<|> update
           :<|> delete
  where
    insert :: HC.ApiDocument -> HuntResult ()
    insert doc = evalCmdWith env cmdInsert >>= getOkResult
      where
        cmdInsert = HC.cmdInsertDoc doc

    update :: HC.ApiDocument -> HuntResult ()
    update doc = evalCmdWith env cmdUpdate >>= getOkResult
      where
        cmdUpdate = HC.cmdUpdateDoc doc

    delete :: HC.ApiDocument -> HuntResult ()
    delete doc = evalCmdWith env cmdDelete >>= getOkResult
      where
        cmdDelete = HC.cmdDeleteDoc (HC.adUri doc)


-- | Provide server implementation of the status API based
-- on the given Hunt environment
status :: DefHuntEnv -> Server StatusAPI
status env = gc
        :<|> doctable
        :<|> index
        :<|> context
  where
    gc :: HuntResult CmdResult
    gc = evalCmdWith env $ HC.cmdStatus StatusGC

    doctable :: HuntResult CmdResult
    doctable = evalCmdWith env $ HC.cmdStatus StatusDocTable

    index :: HuntResult CmdResult
    index = evalCmdWith env $ HC.cmdStatus StatusIndex

    context :: T.Text -> HuntResult CmdResult
    context = evalCmdWith env . HC.cmdStatus . StatusContext


evaluate :: DefHuntEnv -> Server EvalAPI
evaluate = evalCmdWith


-- | Provide server implementation of the weight API based
-- on the given Hunt environment
weight :: DefHuntEnv -> Server WeightAPI
weight env = weight'
  where
    weight' :: T.Text -> HuntResult (LimitedResult RankedDoc)
    weight' query
      = evalQuery cmdSearch env query >>= getLimitedResult
      where
        cmdSearch
          = HC.setWeightIncluded . HC.setSelectedFields [] . HC.cmdSearch


-- | Provide server implementation of the select API based
-- on the given Hunt environment
select :: DefHuntEnv -> Server SelectAPI
select env = select'
  where
    select' :: T.Text -> HuntResult (LimitedResult RankedDoc)
    select' query
      = evalQuery HC.cmdSelect env query >>= getLimitedResult


html :: Server HtmlAPI
html = quickstart
  :<|> index
  where
    quickstart :: HuntResult Html
    quickstart = return Templ.quickstart

    index :: HuntResult Html
    index = return $ Templ.index 0


-- INTERPRETER HELPERS

type HuntResult a = ExceptT ServantErr IO a

-- | Parse a given query. A fail results in a HTTP 500 error.
parseQuery :: T.Text -> HuntResult HC.Query
parseQuery = eval . HC.parseQuery . T.unpack
  where
    eval (Right query) = return query
    eval (Left _err)   = throwError err500 { errBody = "Could not parse query." }


-- | Evaluate a given command based on the given Hunt environment.
-- An error will result in a HTTP 500 error.
evalCmdWith :: DefHuntEnv -> Command -> HuntResult CmdResult
evalCmdWith env cmd = liftIO (runCmd env cmd) >>= eval
  where
    eval (Right result) = return result
    eval (Left err)    = throwError err500 { errBody = LB.fromStrict (TE.encodeUtf8 (ceMsg err)) }


-- | Evaluate a given Query as a T.Text.
evalQuery :: (HC.Query -> Command) -> DefHuntEnv -> T.Text -> HuntResult CmdResult
evalQuery mkCmd env query
  = parseQuery query >>= return . mkCmd >>= evalCmdWith env


-- | Return a limited result from the given CmdResult. For
-- everything else, throw an error.
getLimitedResult :: CmdResult -> HuntResult (LimitedResult RankedDoc)
getLimitedResult (ResSearch docs) = return docs
getLimitedResult _ = throwError err500 { errBody = "Internal server error" }


-- | Return a completion result from the given CmdResult. For
-- everything else, throw an error.
getCompletionResult :: CmdResult -> HuntResult Suggestion
getCompletionResult (ResSuggestion sug) = return sug
getCompletionResult _ = throwError err500 { errBody = "Internal server error" }


-- | Return a completion result from the given CmdResult. For
-- everything else, throw an error.
getOkResult :: CmdResult -> HuntResult ()
getOkResult ResOK = return ()
getOkResult _ = throwError err500 { errBody = "Internal server error" }


-- LOGGING HELPERS

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
-}

-- | Log a message at 'ERROR' priority.
errorM :: String -> IO ()
errorM = Log.errorM modName

-- | Convenience function to add a log formatter.
withFormatter :: (Monad m, LogHandler r) => m r -> LogFormatter r -> m r
withFormatter h f = liftM (flip setFormatter f) h

-- | Initializes the loggers (stdout, file).
--   Sets the stdout logger to the given priority
--   and sets the path and 'DEBUG' priority for the file logger.
initLoggers :: Priority -> FilePath -> IO ()
initLoggers prio logFilePath = do
  -- formatter
  let defFormatter = simpleLogFormatter "[$time : $loggername : $prio] $msg"

  -- root does not have a priority
  updateGlobalLogger rootLoggerName clearLevel

  -- stdout root logger
  handlerBare <- streamHandler stdout prio `withFormatter` defFormatter
  updateGlobalLogger rootLoggerName (setHandlers [handlerBare])

  -- file logger always at 'DEBUG' level
  handlerFile <- fileHandler logFilePath DEBUG `withFormatter` defFormatter
  updateGlobalLogger rootLoggerName (addHandler handlerFile)
