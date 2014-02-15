{-# LANGUAGE OverloadedStrings #-}

module Hunt.GeoFrontend.Server where

import Data.String (fromString)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Aeson.Types ()

import qualified Web.Scotty.Trans as Scotty
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified Network.Wai.Handler.Warp as W

import qualified System.Log.Logger as Log
import qualified System.Log.Formatter as Log (simpleLogFormatter)
import qualified System.Log.Handler as Log (setFormatter)
import qualified System.Log.Handler.Simple as Log (streamHandler)
import qualified System.IO as System (stdout)

import qualified Hunt.GeoFrontend.Templates as Templates

import Hunt.GeoFrontend.Common
import Hunt.Server.Client (newServerAndManager)
import Paths_geoFrontend

type GeoFrontendError = TL.Text

start :: GeoFrontendConfiguration -> IO ()
start config = do
    sm <- newServerAndManager $ T.pack $ huntUrl config

    -- Note that 'runM' is only called once, at startup.
    let runM m = runGeoReader m sm
        -- 'runActionToIO' is called once per action.
        runActionToIO = runM

    initLoggers $ optLogLevel defaultOptions

    Log.debugM modName "Application start"

    let options = Scotty.Options {Scotty.verbose = 1, Scotty.settings = (W.defaultSettings { W.settingsPort = geoFrontendPort config, W.settingsHost = fromString $ geoFrontendHost config })}

    Scotty.scottyOptsT options runM runActionToIO $ do
        Scotty.middleware Wai.logStdoutDev -- request / response logging
        dispatcher

dispatcher :: Scotty.ScottyT GeoFrontendError GeoServer ()
dispatcher = do
    Scotty.get "/" $ do
        params <- Scotty.params
        renderRoot params
    Scotty.get "/geoFrontend.js" $ do
        Scotty.setHeader "Content-Type" "text/javascript"
        jsPath <- liftIO $ getDataFileName "geoFrontend.js"
        Scotty.file jsPath
    Scotty.get "/geoFrontend.css" $ do
        Scotty.setHeader "Content-Type" "text/css"
        cssPath <- liftIO $ getDataFileName "geoFrontend.css"
        Scotty.file cssPath
    Scotty.get "/autocomplete"$ do
        q <- Scotty.param "term"
        value <- (lift $ autocomplete $ TL.toStrict q) >>= raiseOnLeft
        Scotty.json $ value


renderRoot :: [Scotty.Param] -> Scotty.ActionT GeoFrontendError GeoServer ()
renderRoot params = renderRoot' $ (fmap TL.toStrict) $ lookup "query" params
    where 
    renderRoot' :: Maybe T.Text -> Scotty.ActionT GeoFrontendError GeoServer ()
    renderRoot' Nothing = Scotty.html $ Templates.body ""
    renderRoot' (Just q) = do
        value <- (lift $ query q) >>= raiseOnLeft
        Scotty.html $ Templates.body (TL.fromStrict q)

-- raiseOnLeft :: Monad m => Either T.Text a -> Scotty.ActionT GeoFrontendError m a
raiseOnLeft (Left err) = Scotty.raise $ TL.fromStrict err
raiseOnLeft (Right x) = return x
    
-- | Set the body of the response to the given 'T.Text' value. Also sets \"Content-Type\"
-- header to \"text/html\".
javascript :: (Scotty.ScottyError e, Monad m) => T.Text -> Scotty.ActionT e m ()
javascript t = do
    Scotty.setHeader "Content-Type" "text/javascript"
    Scotty.raw $ TL.encodeUtf8 $ TL.fromStrict t


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
modName = "GeoFrontend"