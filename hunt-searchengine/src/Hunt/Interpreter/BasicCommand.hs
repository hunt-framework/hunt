{-# LANGUAGE OverloadedStrings #-}

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

-- ----------------------------------------------------------------------------

-- | The (low level) commands that actually get interpreted.
data BasicCommand
  -- | Search
  = Search        { icQuery    :: Query
                  , icOffsetSR :: Int
                  , icMaxSR    :: Int
                  }
  | Completion    { icPrefixCR :: Query
                  , icMaxCR    :: Int
                  }

  -- | Index manipulation
  | InsertList    { icDocs :: [ApiDocument] }
  | Update        { icDoc :: ApiDocument }
  | DeleteDocs   { icUris :: Set URI }
  | DeleteByQuery { icQueryD :: Query }

  -- | context manipulation
  | InsertContext { icICon   :: Context
                  , icSchema :: ContextSchema
                  }
  | DeleteContext { icDCon :: Context }

  -- | persistent commands
  | LoadIx        { icPath :: FilePath }
  | StoreIx       { icPath :: FilePath }

  -- | status
  | Status        { icStatus :: StatusCmd }

  -- | general
  | Sequence      { icCmdSeq :: [BasicCommand] }
  | NOOP
  deriving (Show)


data StatusCmd
  = StatusGC
  | StatusDocTable
  | StatusIndex
    deriving (Show)

-- ----------------------------------------------------------------------------
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

-- ----------------------------------------------------------------------------

instance LogShow BasicCommand where
  logShow (InsertList docs) = "InsertList " ++ show (map apiDocUri docs)
  logShow (Update doc) = "Update {icDoc = " ++ logShow doc ++ "\", ..}"
  logShow (Sequence _) = "Sequence"
  logShow o = show o

-- ----------------------------------------------------------------------------
