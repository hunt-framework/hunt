{-# LANGUAGE OverloadedStrings #-}

module Hunt.Interpreter.Command
( Command (..) , StatusCmd (..)
, CmdResult (..), CmdError (..)
, toBasicCommand
)
where

import           Control.Monad                 (mzero)
import           Control.Monad.Error           (Error (..))

import           Data.Aeson
import           Data.List
import qualified Data.Set                      as S
import           Data.Text                     (Text)
import qualified Data.Text                     as T

import           Hunt.Common.ApiDocument
import           Hunt.Common.BasicTypes
import           Hunt.Common.Document          (Document)
import           Hunt.Index.Schema
import           Hunt.Query.Language.Grammar   (Query (..))

import           Hunt.Interpreter.BasicCommand (BasicCommand, StatusCmd (..))
import qualified Hunt.Interpreter.BasicCommand as Cmd

import           Hunt.Utility.Log

import           Hunt.Query.Result

-- ----------------------------------------------------------------------------

-- | The commands of the the 'Interpreter'.
--   These are actually translated to 'BasicCommand's.
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
  | Insert        { icDoc :: ApiDocument }
  | Update        { icDoc :: ApiDocument }
  | Delete        { icUri :: URI }
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
  | Sequence      { icCmdSeq :: [Command] }
  | NOOP
  deriving (Show)

data CmdResult
  = ResOK
  | ResSearch       { crRes   :: LimitedResult (Document, Score) }
  | ResCompletion   { crWords :: [(Text, [Text])] }
  | ResGeneric      { crGen   :: Value }
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
  logShow (Sequence _) = "Sequence"
  logShow o = show o

-- ----------------------------------------------------------------------------

instance ToJSON Command where
  toJSON o = case o of
    Search q ofs mx   -> object . cmd "search"         $ [ "query" .= q, "offset" .= ofs, "max" .= mx ]
    Completion s mx   -> object . cmd "completion"     $ [ "text"  .= s, "max" .= mx ]
    Insert d          -> object . cmd "insert"         $ [ "document" .= d ]
    Update d          -> object . cmd "update"         $ [ "document" .= d ]
    Delete u          -> object . cmd "delete"         $ [ "uri" .= u ]
    DeleteByQuery q   -> object . cmd "delete-by-query"$ [ "query" .= q ]
    InsertContext c s -> object . cmd "insert-context" $ [ "context" .= c, "schema" .= s ]
    DeleteContext c   -> object . cmd "delete-context" $ [ "context" .= c ]
    LoadIx  f         -> object . cmd "load"           $ [ "path" .= f ]
    StoreIx f         -> object . cmd "store"          $ [ "path" .= f ]
    Status  sc        -> object . cmd "status"         $ [ "status" .= sc ]
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
      "delete-by-query"-> o .: "query"    >>= return . DeleteByQuery
      "insert-context" -> do
        cx  <- o .: "context"
        s   <- o .: "schema"
        return $ InsertContext cx s
      "delete-context" -> o .: "context" >>= return . DeleteContext
      "load"           -> o .: "path"    >>= return . LoadIx
      "store"          -> o .: "path"    >>= return . StoreIx
      "noop"           ->                    return NOOP
      "status"         -> o .: "status"  >>= return . Status
      _                -> mzero
  parseJSON o = parseJSON o >>= return . Sequence

instance ToJSON CmdResult where
  toJSON o = case o of
    ResOK           -> object . code 0 $ []
    ResSearch r     -> object . code 0 $ [ "res" .= r ]
    ResCompletion w -> object . code 0 $ [ "res" .= w ]
    ResGeneric v    -> object . code 0 $ [ "res" .= v ]
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

-- ----------------------------------------------------------------------------

-- TODO: - flattening of 'Sequence's?

-- | Transform the supported input command into lower level commands which are actually interpreted.
--   Transformations:
--     - Multiple 'Cmd.Delete's into a single 'DeleteDocs'.
--     - Multiple 'Cmd.Insert's into a single or multiple 'InsertList's.
--       The split is hardcoded to 200 at the moment (see 'splitBatch').
toBasicCommand :: Command -> BasicCommand
toBasicCommand (Sequence cs) = Cmd.Sequence $ opt cs
  where
  opt :: [Command] -> [BasicCommand]
  opt cs' = concatMap optGroup $ groupBy equalHeads cs'
  -- requires the commands to be grouped by constructor
  optGroup :: [Command] -> [BasicCommand]
  -- groups of delete to DeleteDocs
  optGroup cs'@(Delete{}:_)
    = [foldl (\(Cmd.DeleteDocs us) (Delete u)
                -> Cmd.DeleteDocs (S.insert u us)) (Cmd.DeleteDocs S.empty) cs']
  -- groups of Insert to InsertList
  optGroup cs'@(Insert{}:_)
    = [splitBatch 500 $ foldl (\(Cmd.InsertList us) (Insert u)
                -> Cmd.InsertList (u:us)) (Cmd.InsertList []) cs']
  optGroup cs'@(Sequence{}:_)
    = map toBasicCommand cs'
  optGroup cs'
    = map toBasicCommand cs'
  -- group by constructor
  -- NOTE: add constructors to use in optGroup
  equalHeads :: Command -> Command -> Bool
  equalHeads Delete{}   Delete{}   = True
  equalHeads Insert{}   Insert{}   = True
  equalHeads Sequence{} Sequence{} = True
  equalHeads _ _                   = False

toBasicCommand (Delete u)          = Cmd.DeleteDocs $ S.singleton u
toBasicCommand (DeleteByQuery q)   = Cmd.DeleteByQuery q
toBasicCommand (Search a b c)      = Cmd.Search a b c
toBasicCommand (Completion a b)    = Cmd.Completion a b
toBasicCommand (Insert a)          = Cmd.InsertList [a]
toBasicCommand (Update a)          = Cmd.Update a
toBasicCommand (InsertContext a b) = Cmd.InsertContext a b
toBasicCommand (DeleteContext a)   = Cmd.DeleteContext a
toBasicCommand (LoadIx a)          = Cmd.LoadIx a
toBasicCommand (StoreIx a)         = Cmd.StoreIx a
toBasicCommand (Status a)          = Cmd.Status a
toBasicCommand (NOOP)              = Cmd.NOOP

-- | Splits big batch inserts to smaller ones with at most @n@ elements.
--   This can avoid running out of memory for large 'InsertList's.
splitBatch :: Int -> BasicCommand -> BasicCommand
splitBatch n (Cmd.InsertList xs)
    = Cmd.Sequence $ map Cmd.InsertList $ splitEvery n xs
splitBatch _ cmd
    = cmd

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
  (first,rest) = splitAt n list
