module Holumbus.Server.Common where

import           Control.Monad         (mzero)
import           Control.Monad.Error   (Error(..))

import           Data.Monoid           (mappend)

import           Data.Aeson
import           Data.Map              (Map ())
import qualified Data.Map              as M
import           Data.Text             (Text)
import qualified Data.Text             as T

import           Holumbus.Index.Common (Content, Context, Description, Position,
                                        URI, WordList, DocumentRaw)

-- ----------------------------------------------------------------------------

-- | Multiple ApiDocuments.
type ApiDocuments = [ApiDocument]

-- | The document accepted via the API.
data ApiDocument  = ApiDocument
  { apiDocUri      :: URI
  , apiDocIndexMap :: Map Context (Either WordList TextData)
  , apiDocDescrMap :: Description
  }
  deriving (Show)

-- | Data necessary for adding documents to the index.
data TextData = TextData
  { idContent  :: Content
  , idMetadata :: IndexMetadata
  }
  deriving (Show)

-- | Metadata for index processing
data IndexMetadata = IndexMetadata
  { imAnalyzer :: AnalyzerType
  }
  deriving (Show)

-- | Text analysis function
type AnalyzerFunction = Text -> [(Position, Text)]

-- | Types of analyzer
data AnalyzerType
  = DefaultAnalyzer
  deriving (Show)

  -- | The default Matadata
defaultIndexMetadata :: IndexMetadata
defaultIndexMetadata = IndexMetadata
  { imAnalyzer = DefaultAnalyzer
  }

-- |  some sort of json response format
data JsonResponse r = JsonSuccess r | JsonFailure [Text]

-- | paged api document result
data PagedResult x = PagedResult
  { result  :: [x]
  , page    :: Int
  , perPage :: Int
  , count   :: Int
  }

mkPagedResult :: [x] -> Int -> Int -> PagedResult x
mkPagedResult xs p pp = PagedResult
  { result  = takePage
  , page    = p
  , perPage = pp
  , count   = length xs
  }
  where
  takePage = take pp $ drop (pp * (p-1)) xs

-- | empty document
emptyApiDocIndexMap :: Map Context (Either WordList TextData)
emptyApiDocIndexMap = M.empty

emptyApiDocDescrMap :: Description
emptyApiDocDescrMap = M.empty

emptyApiDoc :: ApiDocument
emptyApiDoc = ApiDocument "" emptyApiDocIndexMap emptyApiDocDescrMap

-- ----------------------------------------------------------------------------

instance (ToJSON x) => ToJSON (PagedResult x) where
   toJSON (PagedResult l p pp c) = object
    [ "result"  .= l
    , "page"    .= p
    , "perPage" .= pp
    , "count"   .= c
    ]

instance FromJSON ApiDocument where
  parseJSON (Object o) = do
    parsedUri         <- o    .: "uri"
    indexMap          <- o    .:? "index"       .!= emptyApiDocIndexMap
    descrMap          <- o    .:? "description" .!= emptyApiDocDescrMap
    return ApiDocument
      { apiDocUri       = parsedUri
      , apiDocIndexMap  = indexMap
      , apiDocDescrMap  = descrMap
      }
  parseJSON _ = mzero

instance FromJSON (Either WordList TextData) where
  parseJSON o =
    (parseJSON o >>= return . Left)
    `mappend`
    (parseJSON o >>= return . Right)

instance FromJSON TextData where
  parseJSON (Object o) = do
    content           <- o    .:  "content"
    metadata          <- o    .:? "metadata" .!= defaultIndexMetadata
    return TextData
      { idContent       = content
      , idMetadata      = metadata
      }
  parseJSON _ = mzero

instance FromJSON IndexMetadata where
  parseJSON (Object o) = do
    analyzer <- o .: "analyzer" .!= DefaultAnalyzer
    return IndexMetadata
      { imAnalyzer = analyzer
      }
  parseJSON _ = mzero

instance FromJSON AnalyzerType where
  parseJSON (String s) =
    case s of
      "default" -> return DefaultAnalyzer
      _         -> mzero
  parseJSON _ = mzero

instance ToJSON ApiDocument where
  toJSON (ApiDocument u im dm) = object
    [ "uri"         .= u
    , "index"       .= im
    , "description" .= dm
    ]

instance ToJSON (Either WordList TextData) where
  toJSON = either toJSON toJSON

instance ToJSON TextData where
  toJSON (TextData c m) = object
    [ "content"     .= c
    , "metadata"    .= m
    ]

instance ToJSON IndexMetadata where
  toJSON (IndexMetadata a) = object
    [ "analyzer"    .= a
    ]

instance ToJSON AnalyzerType where
  toJSON (DefaultAnalyzer) =
    "default"

instance (ToJSON r) => ToJSON (JsonResponse r) where
  toJSON (JsonSuccess msg) = object
    [ "code"  .= (0 :: Int)
    , "msg"   .= msg
    ]

  toJSON (JsonFailure msg) = object
    [ "code"  .= (1 :: Int)
    , "msg"   .= msg
    ]

-- ----------------------------------------------------------------------------
-- Interpreter
-- ----------------------------------------------------------------------------

-- simple for now - query stuff needs work...
data Query
  = QWord    Text
  | QPrefix  Text
  | QAnd     Query Query
    deriving (Show)

data InsertOption
  = New | Replace | Modify
    deriving (Show)

data Command
  = Search     { icQuery    :: Query }
  | Completion { icPrefix   :: Text }
  | Insert     { icDoc      :: ApiDocument
               , icInsOpt  :: InsertOption
               }
  | Delete     { icUri      :: URI }
  | LoadIx     { icPath     :: FilePath }
  | StoreIx    { icPath     :: FilePath }
  | Sequence   { icCmdSeq   :: [Command] }
  | NOOP
  deriving (Show)

data CmdResult
  = ResOK
  | ResSearch       { crRes   :: [DocumentRaw] }
  | ResCompletion   { crWords :: [Text] }
  deriving (Show)

data CmdError
  = ResError
    { ceCode :: Int
    , ceMsg  :: Text
    } deriving (Show)

-- ----------------------------------------------------------------------------

instance ToJSON Query where
  toJSON o = case o of
    QWord s     -> object [ "word"     .= s ]
    QPrefix s   -> object [ "prefix"   .= s ]
    QAnd q1 q2  -> object
      [ "q1" .= q1
      , "q2" .= q2 ]

instance FromJSON Query where
  parseJSON (Object o) = do
    t <- o .: "type"
    case (t :: Text) of
      "word"    -> o .: "text" >>= return . QWord
      "prefix"  -> o .: "text" >>= return . QPrefix
      "and" -> do
        q1 <- o .: "q1"
        q2 <- o .: "q2"
        return $ QAnd q1 q2
      _         -> mzero
  parseJSON _ = mzero

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
    Search q      -> object . cmd "search"      $ [ "query" .= q ]
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
      "search"      -> o .: "query" >>= return . Search
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

-- XXX: FromJSON for a result needed?

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
