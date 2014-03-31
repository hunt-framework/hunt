{-# LANGUAGE OverloadedStrings #-}

-- ----------------------------------------------------------------------------
{- |
  Basic \"low-level\" commands that are directly interpreted.

  "Hunt.Interpreter.Command" defines the \"high-level\" commands accepted by the (JSON) API.
-}
-- ----------------------------------------------------------------------------

module Hunt.Interpreter.BasicCommand
  ( BasicCommand (..)
  , StatusCmd (..)
  )
where

import           Control.Monad               (mzero)

import           Data.Aeson
import qualified Data.Aeson                  as JS (Value (..))
import           Data.Set                    (Set)

import           Hunt.Common.ApiDocument
import           Hunt.Common.BasicTypes
import           Hunt.Index.Schema
import           Hunt.Query.Language.Grammar (Query (..))

import           Hunt.Utility.Log

-- ------------------------------------------------------------

-- | The \"low-level\" commands that are actually interpreted.
data BasicCommand
  -- | Search query with pagination.
  = Search        { icQuery    :: Query
                  , icOffsetSR :: Int
                  , icMaxSR    :: Int
                  }
  -- | Auto-completion query with a limit.
  | Completion    { icPrefixCR :: Query
                  , icMaxCR    :: Int
                  }

  -- | Insert documents.
  | InsertList    { icDocs :: [ApiDocument] }
  -- | Update the document description.
  | Update        { icDoc :: ApiDocument }
  -- | Delete documents by 'URI'.
  | DeleteDocs   { icUris :: Set URI }
  -- | Delete all documents of the query result.
  | DeleteByQuery { icQueryD :: Query }

  -- | Insert a context and the associated schema.
  | InsertContext { icICon   :: Context
                  , icSchema :: ContextSchema
                  }
  -- | Delete a context.
  | DeleteContext { icDCon :: Context }

  -- | Deserialize the index.
  | LoadIx        { icPath :: FilePath }
  -- | Serialize the index.
  | StoreIx       { icPath :: FilePath }

  -- | Query general information about the server/index.
  | Status        { icStatus :: StatusCmd }

  -- | Sequence commands.
  | Sequence      { icCmdSeq :: [BasicCommand] }
  -- | No operation. Can be used in control flow and as an alive test.
  | NOOP
  deriving (Show)

-- | Available status commands.
data StatusCmd
  = StatusGC       -- ^ Garbage collection statistics.
  | StatusDocTable -- ^ Document table JSON dump.
  | StatusIndex    -- ^ Index JSON dump.
    deriving (Show)

-- ------------------------------------------------------------
-- XXX: maybe duplicate StatusCmd to keep this module clean

instance ToJSON StatusCmd where
  toJSON StatusGC       = JS.String "gc"
  toJSON StatusDocTable = JS.String "doctable"
  toJSON StatusIndex    = JS.String "index"

instance FromJSON StatusCmd where
  parseJSON (JS.String "gc"      ) = return StatusGC
  parseJSON (JS.String "doctable") = return StatusDocTable
  parseJSON (JS.String "index"   ) = return StatusIndex
  parseJSON _                   = mzero

-- ------------------------------------------------------------

instance LogShow BasicCommand where
  logShow (InsertList docs) = "InsertList " ++ show (map adUri docs)
  logShow (Update doc) = "Update {icDoc = " ++ logShow doc ++ "\", ..}"
  logShow (Sequence _) = "Sequence"
  logShow o = show o

-- ------------------------------------------------------------
