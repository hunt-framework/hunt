{-# LANGUAGE OverloadedStrings #-}
module Hayoo.HolumbusClient where

import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO)
import Data.Text.Lazy (Text)
-- import qualified Data.Text.Lazy as T

import Data.Aeson

import qualified Holumbus.Server.Client as H



data SearchResult = FunctionResult {
    uri :: Text, 
    functionPackage :: Text,
    functionModule :: Text,
    functionName :: Text,
    functionSignature :: Text,
    functionDescription :: Text,
    functionSource :: Text
} deriving (Show, Eq)

instance FromJSON SearchResult where
    parseJSON (Object v) = do
        u <- v .: "uri" 
        (Object descr) <- v .: "desc"
        p  <- descr .: "package"
        m  <- descr .: "module"
        n  <- descr .: "name"
        s  <- descr .: "signature"
        d  <- descr .:? "description" .!= ""
        c  <- descr .:? "source" .!= ""
        return $ FunctionResult u p m n s d c
    parseJSON _ = mzero

autocomplete :: (MonadIO m) => Text -> Text  -> m (Either Text [Text])
autocomplete = H.autocomplete

query :: (MonadIO m) => Text -> Text -> m (Either Text (H.LimitedResult SearchResult))
query = H.query