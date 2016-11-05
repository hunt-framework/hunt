{-# LANGUAGE OverloadedStrings     #-}
module Hunt.Client
  ( -- * Client
    withBaseUrl
  , runClientM

    -- * API

    -- Search
  , search
  , searchText
  , search'

    -- Completion
  , complete
  , completeAll

    -- Documents
  , insertDoc, updateDoc, removeDoc

    -- Eval
  , eval

    -- Weight
  , getWeight

    -- Select
  , select

    -- Status
  , gcStatus, doctableStatus, indexStatus, contextStatus
  ) where


import           Control.Monad.Except
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8    as LBS
import qualified Data.Text               as T
import           Hunt.API
import           Hunt.ClientInterface
import           Hunt.Query.Intermediate (RankedDoc)
import           Network.HTTP.Client     (newManager, defaultManagerSettings)
import           Servant.API
import           Servant.Client          (BaseUrl, ClientEnv (..), ClientM,
                                          ServantError (DecodeFailure), client, runClientM)


-- CLIENT

withBaseUrl :: (MonadIO m) => BaseUrl -> m ClientEnv
withBaseUrl baseUrl = do
  man <- liftIO $ newManager defaultManagerSettings
  return $ ClientEnv man baseUrl


-- SEARCH

search :: (FromJSON a) => Query -> Maybe Offset -> Maybe Limit -> ClientM (LimitedResult a)
search query offset limit = do
  result <- search' query offset limit
  let encoded = encode result
  either (decodeFailure encoded) return $ eitherDecode encoded
  where
    decodeFailure :: LBS.ByteString -> String -> ClientM a
    decodeFailure body err =
      throwError $ DecodeFailure err "application/json" body


searchText :: (FromJSON a) => T.Text -> Maybe Offset -> Maybe Limit -> ClientM (LimitedResult a)
searchText query offset limit =
  case parseQuery $ T.unpack query of
    Left err ->
      throwError $ DecodeFailure (T.unpack err) "text/plain" (LBS.pack $ T.unpack query)

    Right query' ->
      search query' offset limit


-- | Search for the given query and restrict the result by starting
-- from @offset@ only including @limit@ results. For an unlimited number
-- of results @offset@ and @limit@ may be @Nothing@.
search' :: Query -> Maybe Offset -> Maybe Limit -> ClientM (LimitedResult RankedDoc)


-- COMPLETION

-- | Provide a completion limited by @limit@.
complete :: T.Text -> Maybe Limit -> ClientM Suggestion

-- | Provide an unlimited number of completions.
completeAll :: T.Text -> ClientM Suggestion
completeAll query = complete query Nothing


-- DOCUMENTS

-- | Insert the given @document@ into the current index.
insertDoc :: ApiDocument -> ClientM ()

-- | Update the given @document@ in the current index.
updateDoc :: ApiDocument -> ClientM ()

-- | Remove the given @document@ from the current index.
removeDoc :: ApiDocument -> ClientM ()


-- EVAL

-- | Evaluate an arbitrary @command@ and return the
-- resulting @CmdResult@.
eval :: Command -> ClientM CmdResult


-- WEIGHT

-- | Search for documents satisfying the given @query@
-- and provide a weighted result.
getWeight :: T.Text -> ClientM (LimitedResult RankedDoc)


-- SELECT

-- | Select an unlimited number of results for the given @query@.
select :: T.Text -> ClientM (LimitedResult RankedDoc)


-- STATUS

-- | Request the GC status.
gcStatus :: ClientM CmdResult

-- | Request the status of the DocTable.
doctableStatus :: ClientM CmdResult

-- | Request the status of the current index.
indexStatus :: ClientM CmdResult

-- | Request the status of a context identified by
-- the given name. This is experimental.
contextStatus :: T.Text -> ClientM CmdResult


-- CLIENT

(search'
 :<|> complete
 :<|> (insertDoc :<|> updateDoc :<|> removeDoc)
 :<|> eval
 :<|> getWeight
 :<|> select
 :<|> (gcStatus :<|> doctableStatus :<|> indexStatus :<|> contextStatus)) = client huntAPI

