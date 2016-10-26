{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Hunt.Client
  ( -- Requests
    HuntRequest
  , request

    -- Search
  , search
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
import qualified Data.Text               as T
import qualified Data.Text.Lazy.Encoding as T
import           Hunt.API
import           Servant.Client

import           Hunt.ClientInterface
import           Hunt.Query.Intermediate (RankedDoc)
import           Network.HTTP.Client
import           Servant.API


-- TYPES

-- | The HuntClient is just a shorthand for a function which is
-- able to query the HuntAPI provided a Manager and a BaseUrl.
type HuntRequest a = Manager -> BaseUrl -> ExceptT ServantError IO a


-- | Runs the given @HuntRequet@ against the provided BaseUrl, with
-- the default newManager.
request :: BaseUrl -> HuntRequest a -> ExceptT ServantError IO a
request baseUrl req = do
  manager <- lift $ newManager defaultManagerSettings
  req manager baseUrl


-- SEARCH

-- | Search for the given query and restrict the result by starting
-- from @offset@ only including @limit@ results. For an unlimited number
-- of results @offset@ and @limit@ may be @Nothing@.
search :: T.Text -> Maybe Offset -> Maybe Limit -> HuntRequest (LimitedResult RankedDoc)

-- | Search for the given query with an unlimited number of results.
search' :: T.Text -> HuntRequest (LimitedResult RankedDoc)
search' query = search query Nothing Nothing


-- COMPLETION

-- | Provide a completion limited by @limit@.
complete :: T.Text -> Maybe Limit -> HuntRequest Suggestion

-- | Provide an unlimited number of completions.
completeAll :: T.Text -> HuntRequest Suggestion
completeAll query = complete query Nothing


-- DOCUMENTS

-- | Insert the given @document@ into the current index.
insertDoc :: ApiDocument -> HuntRequest ()

-- | Update the given @document@ in the current index.
updateDoc :: ApiDocument -> HuntRequest ()

-- | Remove the given @document@ from the current index.
removeDoc :: ApiDocument -> HuntRequest ()


-- EVAL

-- | Evaluate an arbitrary @command@ and return the
-- resulting @CmdResult@.
eval :: Command -> HuntRequest CmdResult


-- WEIGHT

-- | Search for documents satisfying the given @query@
-- and provide a weighted result.
getWeight :: T.Text -> HuntRequest (LimitedResult RankedDoc)


-- SELECT

-- | Select an unlimited number of results for the given @query@.
select :: T.Text -> HuntRequest (LimitedResult RankedDoc)


-- STATUS

-- | Request the GC status.
gcStatus :: HuntRequest CmdResult

-- | Request the status of the DocTable.
doctableStatus :: HuntRequest CmdResult

-- | Request the status of the current index.
indexStatus :: HuntRequest CmdResult

-- | Request the status of a context identified by
-- the given name. This is experimental.
contextStatus :: T.Text -> HuntRequest CmdResult


-- CLIENT

(search
 :<|> complete
 :<|> (insertDoc :<|> updateDoc :<|> removeDoc)
 :<|> eval
 :<|> getWeight
 :<|> select
 :<|> (gcStatus :<|> doctableStatus :<|> indexStatus :<|> contextStatus)) = client huntAPI

