{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}

module Holumbus.Server.Schrotty
( module Web.Scotty.Trans
, schrotty
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
import           Network.Wai.Handler.Warp     (Port)

import           Web.Scotty.Trans             hiding (jsonData, param)
import qualified Web.Scotty.Trans             as Scotty
--import           Web.Scotty.Util

import           Data.ByteString.Lazy         (ByteString)
import qualified Data.ByteString.Lazy.Char8   as BSL
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Encoding      as TEnc

--import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Aeson                   as A
import           Data.Aeson.Encode.Pretty     as AP

import           Holumbus.Interpreter.Command (CmdError(..))
import           Holumbus.Server.Common

-- ----------------------------------------------------------------------------

newtype WebErrorM a = WebErrorM { runWebErrorM :: ErrorT WebError IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadError WebError)

type Schrotty = ScottyT WebErrorM

data WebError
  = InterpreterError CmdError
  | NotFound
  | JsonInvalid
  | MissingParam     ByteString
  | Other            ByteString
  | Text             Int ByteString
  | forall a . ToJSON a =>
    Json             Int a
-- XXX: no Show instance because it's not a Haskell98 type

instance Error WebError where
    strMsg = Other . BSL.pack

-- ----------------------------------------------------------------------------

handleCustomError :: Monad m => Int -> ByteString -> m Response
handleCustomError code bs = return $ plainResponse (toEnum code) bs


handleError :: WebError -> IO Response
handleError (NotFound)
  = return $ plainResponse status404 $ "Not found."
handleError (JsonInvalid)
  = return $ jsonResponse status400 $ JsonFailure 400 ("json invalid"::String)
handleError (MissingParam bs)
  = return $ plainResponse status404 $ "Query parameter missing " `BSL.append` bs
handleError (Other bs)
  = return $ plainResponse status500 bs
handleError (Text code bs)
  = handleCustomError code bs
handleError (Json code o)
  = return $ jsonResponse (toEnum code) (JsonFailure code (A.encode o))
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
jsonData :: FromJSON a => ActionT WebErrorM a
jsonData = Scotty.jsonData
            `rescue` (\_ -> throw JsonInvalid)

-- | Replacement for 'Web.Scotty.param' with custom error.
param :: Parsable a => TL.Text -> ActionT WebErrorM a
param p = Scotty.param p
            `rescue` (\_ -> throw $ MissingParam $ TEnc.encodeUtf8 p)

-- ----------------------------------------------------------------------------

schrotty :: Port -> Schrotty () -> IO ()
schrotty p = Scotty.scottyT p runM runActionToIO
  where
  runM m = do
    r <- runErrorT (runWebErrorM m)
    either (\_ex -> fail $ "exception at startup") return r
  -- 'runActionToIO' is called once per action
  runActionToIO m = runErrorT (runWebErrorM m) >>= either handleError return
