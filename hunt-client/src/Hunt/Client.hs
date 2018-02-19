{-# LANGUAGE OverloadedStrings #-}
module Hunt.Client
  ( -- * Client
    withBaseUrl
  , huntBaseUrl
  , runClientM

    -- * API

    -- Search
  , search
  , searchText
  , search'

    -- Completion
  , complete
  , completeText
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
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Sequence              as Seq
import qualified Data.Text                  as T
import           Hunt.API
import           Hunt.ClientInterface
import           Hunt.Query.Intermediate    (RankedDoc)
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Network.HTTP.Types.Header  (hContentType)
import           Network.HTTP.Types.Status  (status400, status500)
import           Network.HTTP.Types.Version (http11)
import           Servant.API
import           Servant.Client             (BaseUrl (BaseUrl), ClientEnv (..),
                                             ClientM, GenResponse (..),
                                             Response, Scheme (Http),
                                             ServantError (DecodeFailure),
                                             client, runClientM)


-- CLIENT

huntBaseUrl :: BaseUrl
huntBaseUrl = BaseUrl Http "localhost" 3000 ""


withBaseUrl :: (MonadIO m) => BaseUrl -> m ClientEnv
withBaseUrl baseUrl = do
  man <- liftIO $ newManager defaultManagerSettings
  pure $ ClientEnv man baseUrl Nothing


-- SEARCH

search :: (FromJSON a) => Query -> Maybe Offset -> Maybe Limit -> ClientM (LimitedResult a)
search query offset limit = do
  result <- search' query offset limit
  let encoded = encode result
  either (decodeFailure encoded) return $ eitherDecode encoded
  where
    decodeFailure :: LBS.ByteString -> String -> ClientM a
    decodeFailure body err =
      let
        headers =
          Seq.singleton (hContentType, "application/json")
      in
        throwError $ DecodeFailure (T.pack err) (Response status500 headers http11 body)


searchText :: (FromJSON a) => T.Text -> Maybe Offset -> Maybe Limit -> ClientM (LimitedResult a)
searchText queryText offset limit = do
  query <- parseQuery' queryText
  search query offset limit


-- | Search for the given query and restrict the result by starting
-- from @offset@ only including @limit@ results. For an unlimited number
-- of results @offset@ and @limit@ may be @Nothing@.
search' :: Query -> Maybe Offset -> Maybe Limit -> ClientM (LimitedResult RankedDoc)


-- COMPLETION

completeText :: T.Text -> Maybe Limit -> ClientM Suggestion
completeText queryText limit = do
  query <- parseQuery' queryText
  complete query limit


-- | Provide a completion limited by @limit@.
complete :: Query -> Maybe Limit -> ClientM Suggestion


-- | Provide an unlimited number of completions.
completeAll :: Query -> ClientM Suggestion
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
getWeight :: Query -> ClientM (LimitedResult RankedDoc)


-- SELECT

-- | Select an unlimited number of results for the given @query@.
select :: Query -> ClientM (LimitedResult RankedDoc)


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


-- HELPERS

parseQuery' :: T.Text -> ClientM Query
parseQuery' query = either handleErr return $ parseQuery $ T.unpack query
  where
    handleErr :: T.Text -> ClientM Query
    handleErr err =
      let
        headers =
          Seq.singleton (hContentType, "text/plain")

        body =
          LBS.pack (T.unpack query)
      in
        throwError $ DecodeFailure err (Response status400 headers http11 body)
