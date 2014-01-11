{-# LANGUAGE OverloadedStrings #-}

module Hayoo.Server where

import qualified Web.Scotty as Scotty
--import qualified Web.Scotty.Trans as ScottyT
import qualified Network.Wai.Middleware.RequestLogger as Wai

import qualified System.Log.Logger as Log
import qualified System.Log.Formatter as Log (simpleLogFormatter)
import qualified System.Log.Handler as Log (setFormatter)
import qualified System.Log.Handler.Simple as Log (streamHandler)
import qualified System.IO as System (stdout)

import qualified Hayoo.Templates as Templates
--import Data.Text.Lazy (pack)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import           Data.Aeson.Types ()
import Control.Monad.IO.Class (liftIO)

import Hayoo.HolumbusClient
import Paths_hayooFrontend

start :: IO ()
start = do 
    initLoggers $ optLogLevel defaultOptions

    Log.debugM modName "Application start"

    Scotty.scotty 8080 $ do
        Scotty.middleware Wai.logStdoutDev -- request / response logging
        dispatcher


dispatcher :: Scotty.ScottyM ()
dispatcher = do
    Scotty.get "/" $ do
        params <- Scotty.params
        renderRoot params
    Scotty.get "/hayoo.js" $ do
        Scotty.setHeader "Content-Type" "text/javascript"
        jsPath <- liftIO $ getDataFileName "hayoo.js"
        Scotty.file jsPath
    Scotty.get "/hayoo.css" $ do
        Scotty.setHeader "Content-Type" "text/css"
        cssPath <- liftIO $ getDataFileName "hayoo.css"
        Scotty.file cssPath
    Scotty.get "/autocomplete"$ do
        q <- Scotty.param "term"
        value <- autocomplete "localhost:3000" q >>= raiseOnLeft
        Scotty.json $ value
    Scotty.get "/examples" $ Scotty.html $ Templates.body "" Templates.examples
    Scotty.get "/about" $ Scotty.html $ Templates.body "" Templates.about


renderRoot :: [Scotty.Param] -> Scotty.ActionM ()
renderRoot params = renderRoot' $ lookup (T.pack "query") params
    where 
    renderRoot' Nothing = Scotty.html $ Templates.body "" Templates.mainPage
    renderRoot' (Just q) = do
        value <- query "localhost:3000" q >>= raiseOnLeft
        Scotty.html $ Templates.body q $ Templates.renderLimitedRestults value

raiseOnLeft :: Either T.Text a -> Scotty.ActionM a
raiseOnLeft (Left err) = Scotty.raise err
raiseOnLeft (Right x) = return x
    
-- | Set the body of the response to the given 'T.Text' value. Also sets \"Content-Type\"
-- header to \"text/html\".
javascript :: T.Text -> Scotty.ActionM ()
javascript t = do
    Scotty.setHeader "Content-Type" "text/javascript"
    Scotty.raw $ T.encodeUtf8 t


-- | Initializes the loggers with the given priority.
initLoggers :: Log.Priority -> IO ()
initLoggers level = do
    handlerBare <- Log.streamHandler System.stdout Log.DEBUG
    let handler = Log.setFormatter handlerBare $ Log.simpleLogFormatter "[$time : $loggername : $prio] $msg"

    Log.updateGlobalLogger "" (Log.setLevel level . Log.setHandlers [handler])
    rl <- Log.getRootLogger
    Log.saveGlobalLogger rl

data Options = Options
  { optLogLevel ::Log.Priority
  }

defaultOptions :: Options
defaultOptions = Options
  { optLogLevel = Log.DEBUG
  }

modName :: String
modName = "HayooFrontend"