{-# LANGUAGE OverloadedStrings #-}

module Holumbus.Interpreter.BasicCommand
( BasicCommand (..), StatusCmd (..)
)
where

import           Control.Monad                   (mzero)

import           Data.Aeson
import qualified Data.Aeson                      as JS (Value (..))
import           Data.Set                        (Set)

import           Holumbus.Common.ApiDocument
import           Holumbus.Common.BasicTypes
import           Holumbus.Index.Schema
import           Holumbus.Query.Language.Grammar (Query (..))

import           Holumbus.Utility.Log

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
  | Insert        { icDoc :: ApiDocument }
  | Update        { icDoc :: ApiDocument }
  | BatchDelete   { icUris :: Set URI }

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
  | StatusIndex
    deriving (Show)

-- ----------------------------------------------------------------------------
-- XXX: maybe duplicate StatusCmd to keep this module clean

instance ToJSON StatusCmd where
    toJSON StatusGC    = JS.String "gc"
    toJSON StatusIndex = JS.String "index"

instance FromJSON StatusCmd where
    parseJSON (JS.String "gc"   ) = return StatusGC
    parseJSON (JS.String "index") = return StatusIndex
    parseJSON _                   = mzero

-- ----------------------------------------------------------------------------

instance LogShow BasicCommand where
  logShow (Insert doc) = "Insert {icDoc = " ++ logShow doc ++ "\", ..}"
  logShow (Update doc) = "Update {icDoc = " ++ logShow doc ++ "\", ..}"
  logShow (Sequence _) = "Sequence"
  logShow o = show o

-- ----------------------------------------------------------------------------
