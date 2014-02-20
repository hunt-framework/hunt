{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DoAndIfThenElse #-}


module Hunt.GeoFrontend.Server 
(
    start
) 
where

import           Data.String (fromString)
import           Data.Maybe (isJust)

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)

import           Data.String.Conversions (cs) -- , (<>)
import qualified Data.Text as T
-- import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
-- import qualified Data.Text.Lazy.Encoding as TL
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

import           Hunt.GeoFrontend.Common
import           Hunt.GeoFrontend.Feeder
import           Hunt.Server.Client (newServerAndManager, withServerAndManager)
import qualified Hunt.Server.Client as H (eval, Command (..))

import           Paths_geoFrontend

type GeoFrontendError = TL.Text

start :: GeoFrontendConfiguration -> IO ()
start config = do
    sm <- newServerAndManager $ cs $ huntUrl config

    -- Note that 'runM' is only called once, at startup.
    let runM m = runGeoReader m sm
        -- 'runActionToIO' is called once per action.
        runActionToIO = runM

    initLoggers $ optLogLevel defaultOptions

    Log.debugM modName "Application start"

    case loadIndex config of
        Just path -> do
            docs <- readXML path
            t <- withServerAndManager (H.eval $ createIndexCommands ++ map (H.Insert . geoDocToHuntDoc) docs) sm
            Log.debugM modName (cs t)
        Nothing -> return ()

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
        value <- (lift $ autocomplete q) >>= raiseOnLeft
        Scotty.json $ value


renderRoot :: [Scotty.Param] -> Scotty.ActionT GeoFrontendError GeoServer ()
renderRoot params = renderRoot' $ (fmap cs) $ lookup "query" params
    where 
    renderRoot' :: Maybe T.Text -> Scotty.ActionT GeoFrontendError GeoServer ()
    renderRoot' Nothing = Scotty.html $ Templates.body ""
    renderRoot' (Just q) = do
        Scotty.html $ Templates.body (cs q)

-- raiseOnLeft :: Monad m => Either T.Text a -> Scotty.ActionT GeoFrontendError m a
raiseOnLeft :: Monad m => Either T.Text a -> Scotty.ActionT TL.Text m a
raiseOnLeft (Left err) = Scotty.raise $ cs err
raiseOnLeft (Right x) = return x
    
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