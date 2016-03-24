{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}

-- ----------------------------------------------------------------------------
{- |
  Schrotty server.
  Scotty with custom error handling.
  All regular scotty functions can be used.
-}
-- ----------------------------------------------------------------------------

module Hunt.Server.Schrotty
       (
         -- * Start the server
         schrotty
       , schrottyOpts

         -- * Adjusted/Added Scotty Functions
       , jsonData
       , param
       , jsonPretty

         -- * Types
       , module Web.Scotty.Trans
       , ActionSchrotty
       , ActionSchrottyT
       , WebError(..)
       )
where

import           Control.Monad.IO.Class
import           Data.Aeson as A
import           Data.Aeson.Encode.Pretty as AP
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import           Hunt.Interpreter.Command (CmdError (..))
import           Hunt.Server.Common
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp (Port)
import qualified Web.Scotty.Trans as Scotty
import           Web.Scotty.Trans hiding (jsonData, param)

-- ------------------------------------------------------------

type ActionSchrottyT m a = ActionT WebError m a

type ActionSchrotty a    = ActionT WebError IO a

data WebError
  = InterpreterError CmdError
  | NotFound
  | JsonInvalid      String
  | MissingParam     Text
  | Other            Text
  | Text             Int Text
  | forall a . ToJSON a =>
    Json             Int a
-- XXX: no Show instance because it's not a Haskell98 type

instance ScottyError WebError where
  stringError = Text 500 . TL.pack
  showError _ = "schrotty exception :("

-- ------------------------------------------------------------

handleCustomError :: Monad m => WebError -> ActionSchrottyT m ()
handleCustomError (NotFound)
  = do
    status status404
    text $ "Not found."
handleCustomError (JsonInvalid s)
  = do
    status status400
    jsonPretty $ JsonFailure 400 ("json invalid: " ++ s)
handleCustomError (MissingParam txt)
  = do
    status status404
    text $ "Query parameter missing " `TL.append` txt -- TODO: fix types
handleCustomError (Other txt)
  = do
    status status500
    text $ txt
handleCustomError (Text code bs)
  = do
    status (toEnum code)
    text $ bs
handleCustomError (Json code o)
  = do
    status (toEnum code)
    jsonPretty $ JsonFailure code o
handleCustomError (InterpreterError (ResError code msg))
  = handleCustomError (Json code msg)

-- | Like 'Web.Scotty.json', but pretty.
jsonPretty :: (ToJSON a, ScottyError e, Monad m) => a -> ActionT e m ()
jsonPretty v = do
  setHeader "Content-Type" "application/json"
  raw $ AP.encodePretty v

-- | Replacement for 'Web.Scotty.jsonData' with custom error.
jsonData :: (FromJSON a, Monad m, MonadIO m) => ActionSchrottyT m a
jsonData = do
  b <- body
  case A.eitherDecode b of
    Right j -> return j
    Left e  -> raise $ JsonInvalid e

-- | Replacement for 'Web.Scotty.param' with custom error.
param :: (Parsable a, Monad m) => TL.Text -> ActionSchrottyT m a
param p = Scotty.param p
            `rescue` (\_ -> raise $ MissingParam p)

-- ------------------------------------------------------------

-- | Start schrotty.
schrotty :: Port -> ScottyT WebError IO () -> IO ()
schrotty p a = Scotty.scottyT p id $ defaultHandler handleCustomError >> a

-- | Start schrotty with options.
schrottyOpts :: Options -> ScottyT WebError IO () -> IO ()
schrottyOpts o a = Scotty.scottyOptsT o id $ defaultHandler handleCustomError >> a

-- ------------------------------------------------------------
