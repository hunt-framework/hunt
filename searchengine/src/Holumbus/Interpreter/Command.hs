module Holumbus.Interpreter.Command where

import           Control.Monad                   (mzero)
import           Control.Monad.Error             (Error (..))

import           Data.Aeson
import           Data.Set                        (Set)
import           Data.Text                       (Text)
import qualified Data.Text                       as T

import           Holumbus.Common.ApiDocument
import           Holumbus.Common.BasicTypes
import           Holumbus.Common.Document        (Document)
import           Holumbus.Query.Language.Grammar (Query (..))
import           Holumbus.Index.Schema

import           Holumbus.Utility.Log

-- ----------------------------------------------------------------------------

type UnparsedQuery = Text

data Command
  -- | Search
  = Search        { icQuery    :: Query
                  , icOffsetSR :: Int
                  , icMaxSR    :: Int
                  }
  | Completion    { icPrefixCR :: Query
                  , icMaxCR    :: Int
                  }
  -- | Index manipulation
  | Insert        { icDoc      :: ApiDocument }
  | Update        { icDoc      :: ApiDocument }
  | Delete        { icUri      :: URI }
  | BatchDelete   { icUris     :: Set URI }
  -- | context manipulation
  | InsertContext { icICon     :: Context
                  , icSchema   :: ContextSchema
                  }
  | DeleteContext { icDCon     :: Context }
  -- | persistent commands
  | LoadIx        { icPath     :: FilePath }
  | StoreIx       { icPath     :: FilePath }
  -- | general
  | Sequence      { icCmdSeq   :: [Command] }
  | NOOP
  deriving (Show)

data CmdResult
  = ResOK
  | ResSearch       { crRes   :: LimitedResult Document }
  | ResCompletion   { crWords :: [Text] }
  deriving (Show, Eq)

data CmdError
  = ResError
    { ceCode :: Int
    , ceMsg  :: Text
    } deriving (Show)

-- ----------------------------------------------------------------------------

instance LogShow Command where
  logShow (Insert doc) = "Insert {icDoc = " ++ logShow doc ++ "\", ..}"
  logShow (Update doc) = "Update {icDoc = " ++ logShow doc ++ "\", ..}"
  logShow o = show o

-- ----------------------------------------------------------------------------

instance ToJSON Command where
  toJSON o = case o of
    Search q ofs mx   -> object . cmd "search"         $ [ "query" .= q, "offset" .= ofs, "max" .= mx ]
    Completion s mx   -> object . cmd "completion"     $ [ "text"  .= s, "max" .= mx ]
    Insert d          -> object . cmd "insert"         $ [ "document" .= d ]
    Update d          -> object . cmd "update"         $ [ "document" .= d ]
    Delete u          -> object . cmd "delete"         $ [ "uri" .= u ]
    InsertContext c s -> object . cmd "insert-context" $ [ "context" .= c, "schema" .= s ]
    DeleteContext c   -> object . cmd "delete-context" $ [ "context" .= c ]
    BatchDelete us    -> object . cmd "delete-batch"   $ [ "uris" .= us ] -- not used in fromJSON instance
    LoadIx  f         -> object . cmd "load"           $ [ "path" .= f ]
    StoreIx f         -> object . cmd "store"          $ [ "path" .= f ]
    NOOP              -> object . cmd "noop"           $ []
    Sequence cs       -> toJSON cs
    where
    cmd c = (:) ("cmd" .= (c :: Text))

instance FromJSON Command where
  parseJSON (Object o) = do
    c <- o .: "cmd"
    case (c :: Text) of
      "search"         -> do
        q  <- o .: "query"
        p  <- o .: "offset"
        pp <- o .: "max"
        return $ Search q p pp
      "completion"     -> do
        txt <- o .: "text"
        mx  <- o .: "max"
        return $ Completion txt mx
      "insert"         -> o .: "document" >>= return . Insert
      "update"         -> o .: "document" >>= return . Update
      "delete"         -> o .: "uri"      >>= return . Delete
      "insert-context" -> do
        cx  <- o .: "context"
        s   <- o .: "schema"
        return $ InsertContext cx s
      "delete-context" -> o .: "context" >>= return . DeleteContext
      "load"           -> o .: "path"    >>= return . LoadIx
      "store"          -> o .: "path"    >>= return . StoreIx
      "noop"           ->                    return NOOP
      _                -> mzero
  parseJSON o = parseJSON o >>= return . Sequence

instance ToJSON CmdResult where
  toJSON o = case o of
    ResOK           -> object . code 0 $ []
    ResSearch r     -> object . code 0 $ [ "res" .= r ]
    ResCompletion w -> object . code 0 $ [ "res" .= w ]
    where
    code i = (:) ("code" .= (i :: Int))

instance Error CmdError where
  strMsg s = ResError 500 . T.pack $ "internal server error: " ++ s

instance ToJSON CmdError where
  toJSON (ResError c m) = object
    [ "code" .= c
    , "msg"  .= m
    ]

instance FromJSON CmdError where
  parseJSON (Object o) = do
    c <- o .: "code"
    m <- o .: "msg"
    return $ ResError c m
  parseJSON _ = mzero
