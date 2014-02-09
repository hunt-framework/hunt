{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}

module Hunt.Server.Schrotty
( module Web.Scotty.Trans
, schrotty
, schrottyOpts
, throw
, jsonData
, param
, jsonPretty
, WebError(..)
)
where

import           Control.Applicative
import           Control.Monad.Error

import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp   (Port)

import           Web.Scotty                 (ActionM)
import           Web.Scotty.Trans           hiding (jsonData, param)
import qualified Web.Scotty.Trans           as Scotty
--import           Web.Scotty.Util

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TEnc

--import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Aeson                 as A
import           Data.Aeson.Encode.Pretty   as AP

import           Hunt.Interpreter.Command   (CmdError (..))
import           Hunt.Server.Common

-- ----------------------------------------------------------------------------

newtype WebErrorM a = WebErrorM { runWebErrorM :: ErrorT WebError IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError WebError)

-- TODO: use ScottyT error stack...
type Schrotty = ScottyT WebError WebErrorM

data WebError
  = InterpreterError CmdError
  | NotFound
  | JsonInvalid      String
  | MissingParam     ByteString
  | Other            ByteString
  | Text             Int ByteString
  | forall a . ToJSON a =>
    Json             Int a
-- XXX: no Show instance because it's not a Haskell98 type

instance Error WebError where
  strMsg = Other . BSL.pack

instance ScottyError WebError where
  stringError = Text 500 . BSL.pack
  showError _ = "schrotty exception"

-- ----------------------------------------------------------------------------

handleCustomError :: Monad m => Int -> ByteString -> m Response
handleCustomError code bs = return $ plainResponse (toEnum code) bs


handleError :: WebError -> IO Response
handleError (NotFound)
  = return $ plainResponse status404 $ "Not found."
handleError (JsonInvalid s)
  = return $ jsonResponse status400 $ JsonFailure 400 ("json invalid: " ++ s)
handleError (MissingParam bs)
  = return $ plainResponse status404 $ "Query parameter missing " `BSL.append` bs
handleError (Other bs)
  = return $ plainResponse status500 bs
handleError (Text code bs)
  = handleCustomError code bs
handleError (Json code o)
  = return $ jsonResponse (toEnum code) (JsonFailure code o)
handleError (InterpreterError (ResError code msg))
  = handleError (Json code msg)
  -- = handleCustomError code $ TEnc.encodeUtf8 . TL.fromStrict $ msg


plainResponse :: Status -> ByteString -> Response
plainResponse st bs = responseLBS st [("Content-type","text/plain; charset=utf-8")] bs

jsonResponse :: ToJSON a => Status -> a -> Response
jsonResponse st js = responseLBS st [("Content-type","application/json")] (A.encode js)

throw :: MonadTrans t => WebError -> t WebErrorM a
throw = lift . throwError

-- | Like 'Web.Scotty.json', but pretty.
jsonPretty :: ToJSON a => a -> ActionM ()
jsonPretty v = do
  setHeader "Content-Type" "application/json"
  raw $ AP.encodePretty v

-- | Replacement for 'Web.Scotty.jsonData' with custom error.
jsonData :: (FromJSON a, ScottyError e) => ActionT e WebErrorM a
jsonData = do
  b <- body
  case A.eitherDecode b of
    (Right j) -> return j
    (Left e)  -> throw (JsonInvalid e)

-- | Replacement for 'Web.Scotty.param' with custom error.
param :: (Parsable a, ScottyError e) => TL.Text -> ActionT e WebErrorM a
param p = Scotty.param p
            `rescue` (\_ -> throw $ MissingParam $ TEnc.encodeUtf8 p)

-- ----------------------------------------------------------------------------
runM :: WebErrorM b -> IO b
runM m = do
  r <- runErrorT (runWebErrorM m)
  either (\_ex -> fail $ "exception at startup") return r

-- | 'runActionToIO' is called once per action
runActionToIO :: WebErrorM Response -> IO Response
runActionToIO m = runErrorT (runWebErrorM m) >>= either handleError return

schrotty :: Port -> Schrotty () -> IO ()
schrotty p = Scotty.scottyT p runM runActionToIO

schrottyOpts :: Options -> Schrotty () -> IO ()
schrottyOpts o = Scotty.scottyOptsT o runM runActionToIO
