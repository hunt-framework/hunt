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

-- ----------------------------------------------------------------------------

data InsertOption
  = New | Replace | Modify
    deriving (Show)

type UnparsedQuery = Text

data Command
  = Search     { icQuery    :: Either UnparsedQuery Query 
               , page       :: Int
               , perPage    :: Int
               }
  | Completion { icPrefix   :: Text }
  | Insert     { icDoc      :: ApiDocument
               , icInsOpt   :: InsertOption
               }
  | Delete     { icUri      :: Set URI }
  | LoadIx     { icPath     :: FilePath }
  | StoreIx    { icPath     :: FilePath }
  | Sequence   { icCmdSeq   :: [Command] }
  | NOOP
  deriving (Show)

data CmdResult
  = ResOK
  | ResSearch       { crRes   :: PagedResult Document }
  | ResCompletion   { crWords :: [Text] }
  deriving (Show, Eq)

data CmdError
  = ResError
    { ceCode :: Int
    , ceMsg  :: Text
    } deriving (Show)

-- ----------------------------------------------------------------------------

instance FromJSON InsertOption where
  parseJSON (String s)
    = case s of
      "new"     -> return New
      "replace" -> return Replace
      "modify"  -> return Modify
      _         -> mzero
  parseJSON _ = mzero

instance ToJSON InsertOption where
  toJSON o = case o of
    New     -> "new"
    Replace -> "replace"
    Modify  -> "modify"

instance ToJSON Command where
  toJSON o = case o of
    Search q p pp -> object . cmd "search"      $ [ "query" .= q, "page" .= p, "perPage" .=pp ]
    Completion s  -> object . cmd "completion"  $ [ "text"  .= s ]
    Insert d op   -> object . cmd "insert"      $
      [ "option"    .= op
      , "document"  .= d
      ]
    Delete u      -> object . cmd "delete"      $ [ "uri"   .= u ]
    LoadIx  f     -> object . cmd "load"        $ [ "path"  .= f ]
    StoreIx f     -> object . cmd "store"       $ [ "path"  .= f ]
    NOOP          -> object . cmd "noop"        $ []
    Sequence cs   -> toJSON cs
    where
    cmd c = (:) ("cmd" .= (c :: Text))

instance FromJSON Command where
  parseJSON (Object o) = do
    c <- o .: "cmd"
    case (c :: Text) of
      "search"      -> do 
        q  <- o .: "query" 
        p  <- o .: "page"
        pp <- o .: "perPage"
        return $ Search q p pp
      "completion"  -> o .: "text"  >>= return . Completion
      "insert"      -> do
        op <- o .: "option"
        d  <- o .: "document"
        return $ Insert d op
      "delete"      -> o .: "uri"   >>= return . Delete
      "load"        -> o .: "path"  >>= return . LoadIx
      "store"       -> o .: "path"  >>= return . StoreIx
      "noop"        ->                  return NOOP
      _             -> mzero
  parseJSON o       = parseJSON o   >>= return . Sequence

instance ToJSON CmdResult where
  toJSON o = case o of
    ResOK -> object . code 0 $ []
    ResSearch r -> object . code 0 $
      [ "res" .= r ]
    ResCompletion w -> object . code 0 $
      [ "res" .= w ]
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
