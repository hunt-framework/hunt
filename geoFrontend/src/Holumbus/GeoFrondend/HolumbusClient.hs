module Holumbus.GeoFrondend.HolumbusClient where

import Control.Monad.IO.Class (MonadIO)
import Data.Text.Lazy (Text)

import qualified Holumbus.Server.Client as H
import Holumbus.GeoFrondend.Common

autocomplete :: (MonadIO m) => Text -> Text  -> m (Either Text [Text])
autocomplete = H.autocomplete

query :: (MonadIO m) => Text -> Text -> m (Either Text (H.LimitedResult GeoDocument))
query = H.query