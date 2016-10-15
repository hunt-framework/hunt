{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Hunt.Client
  ( HuntClient

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
  , gcStatus, doctableStatus, indexStatus, contextStatus, schemaStatus

    -- Index
  , storeIndex, loadIndex

    -- Html
  , quickstart, home
  ) where


import           Control.Monad.Except
import qualified Data.Text               as T
import qualified Data.Text.Lazy.Encoding as T
import           Hunt.API
import           Servant
import           Servant.Client

import           Hunt.ClientInterface
import           Hunt.Query.Intermediate (RankedDoc)
import           Network.HTTP.Client


-- TYPES

-- | The HuntClient is just a shorthand for a function which is
-- able to query the HuntAPI provided a Manager and a BaseUrl.
type HuntClient a = Manager -> BaseUrl -> ExceptT ServantError IO a


-- SEARCH

-- | Search for the given query and restrict the result by starting
-- from @offset@ only including @limit@ results. For an unlimited number
-- of results @offset@ and @limit@ may be @Nothing@.
search :: T.Text -> Maybe Offset -> Maybe Limit -> HuntClient (LimitedResult RankedDoc)

-- | Search for the given query with an unlimited number of results.
search' :: T.Text -> HuntClient (LimitedResult RankedDoc)
search' query = search query Nothing Nothing


-- COMPLETION

-- | Provide a completion limited by @limit@.
complete :: T.Text -> Maybe Limit -> HuntClient Suggestion

-- | Provide an unlimited number of completions.
completeAll :: T.Text -> HuntClient Suggestion
completeAll query = complete query Nothing


-- DOCUMENTS

-- | Insert the given @document@ into the current index.
insertDoc :: ApiDocument -> HuntClient ()

-- | Update the given @document@ in the current index.
updateDoc :: ApiDocument -> HuntClient ()

-- | Remove the given @document@ from the current index.
removeDoc :: ApiDocument -> HuntClient ()


-- EVAL

-- | Evaluate an arbitrary @command@ and return the
-- resulting @CmdResult@.
eval :: Command -> HuntClient CmdResult


-- WEIGHT

-- | Search for documents satisfying the given @query@
-- and provide a weighted result.
getWeight :: T.Text -> HuntClient (LimitedResult RankedDoc)


-- SELECT

-- | Select an unlimited number of results for the given @query@.
select :: T.Text -> HuntClient (LimitedResult RankedDoc)


-- INDEX

-- | Store the current index in a file with the given
-- @filename@.
storeIndex :: T.Text -> HuntClient ()

-- | Replace the current index by loading the one
-- in the given @file@.
loadIndex :: T.Text -> HuntClient ()


-- STATUS

-- | Request the GC status.
gcStatus :: HuntClient CmdResult

-- | Request the status of the DocTable.
doctableStatus :: HuntClient CmdResult

-- | Request the status of the current index.
indexStatus :: HuntClient CmdResult

-- | Request the status of a context identified by
-- the given name. This is experimental.
contextStatus :: T.Text -> HuntClient CmdResult


-- CLIENT

(search
 :<|> complete
 :<|> (insertDoc :<|> updateDoc :<|> removeDoc)
 :<|> eval
 :<|> getWeight
 :<|> select
 :<|> (gcStatus :<|> doctableStatus :<|> indexStatus :<|> contextStatus)
 :<|> (storeIndex :<|> loadIndex)) = client huntAPI

