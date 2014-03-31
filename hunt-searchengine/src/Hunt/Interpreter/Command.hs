{-# LANGUAGE OverloadedStrings #-}

-- ----------------------------------------------------------------------------
{- |
  \"High-level\" commands that are accepted by the (JSON) API.

  These commands are translated with 'toBasicCommand' to 'BasicCommand's which can be interpreted.
-}
-- ----------------------------------------------------------------------------

module Hunt.Interpreter.Command
  ( Command (..)
  , StatusCmd (..)
  , CmdResult (..)
  , CmdError (..)
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

-- ------------------------------------------------------------

-- | The \"high-level\" commands accepted by the 'Interpreter' \/ JSON API.
--   These are translated to 'BasicCommand's.
data Command
  -- | Search query with pagination.
  = Search        { icQuery    :: Query
                  , icOffsetSR :: Int
                  , icMaxSR    :: Int
                  }
  -- | Auto-completion query with a limit.
  | Completion    { icPrefixCR :: Query
                  , icMaxCR    :: Int
                  }

  -- | Insert a document.
  | Insert        { icDoc :: ApiDocument }
  -- | Update a documents' description.
  | Update        { icDoc :: ApiDocument }
  -- | Delete a documents by 'URI'.
  | Delete        { icUri :: URI }
  -- | Delete all documents of the query result.
  | DeleteByQuery { icQueryD :: Query }

  -- | Insert a context and the associated schema.
  | InsertContext { icIContext :: Context
                  , icSchema :: ContextSchema
                  }
  -- | Delete a context.
  | DeleteContext { icDContext :: Context }

  -- | Deserialize the index.
  | LoadIx        { icPath :: FilePath }
  -- | Serialize the index.
  | StoreIx       { icPath :: FilePath }

  -- | Query general information about the server/index.
  | Status        { icStatus :: StatusCmd }

  -- | Sequence commands.
  | Sequence      { icCmdSeq :: [Command] }
  -- | No operation. Can be used in control flow and as an alive test.
  | NOOP
  deriving (Show)

-- | The result of an interpreted command.
data CmdResult
  -- | The command was processed successfully.
  = ResOK
  -- | The search results.
  | ResSearch       { crRes :: LimitedResult (Document, Score) }
  -- | The auto-completion results.
  | ResCompletion   { crWords :: [(Text, [Text])] }
  -- | A generic JSON result.
  | ResGeneric      { crGen :: Value }
  deriving (Show, Eq)

-- | An error during processing of the command.
--   This includes a error code and a message.
data CmdError
  = ResError
    { ceCode :: Int  -- ^ Error code.
    , ceMsg  :: Text -- ^ Message describing the error.
    } deriving (Show)

-- ------------------------------------------------------------

instance LogShow Command where
  logShow (Insert doc) = "Insert {icDoc = " ++ logShow doc ++ "\", ..}"
  logShow (Update doc) = "Update {icDoc = " ++ logShow doc ++ "\", ..}"
  logShow (Sequence _) = "Sequence"
  logShow o = show o

-- ------------------------------------------------------------
-- JSON instances
-- ------------------------------------------------------------

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

-- ------------------------------------------------------------

-- TODO: - flattening of 'Sequence's?

-- | Transform the supported input command into lower level commands which are actually interpreted.
--
--   Transformations:
--
--     - Multiple 'Cmd.Delete's into a single 'DeleteDocs'.
--
--     - Multiple 'Cmd.Insert's into a single or multiple 'InsertList's.
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
  -- TODO: remove constant
  optGroup cs'@(Insert{}:_)
    = [splitBatch 30000000000 $ foldl (\(Cmd.InsertList us) (Insert u)
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
